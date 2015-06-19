use NQPHLL;

grammar Lilikoi::Grammar is HLL::Grammar {
  # Credit here goes to https://github.com/tokuhirom/Perl6-Renshu
  token TOP {
      :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
      :my $*TOP_BLOCK   := $*CUR_BLOCK;
      ^ ~ $ <sexplist>
          || <.panic('Syntax error')>
  }

  token variable { <-[\d\c[APOSTROPHE]\c[QUOTATION MARK]\c[NUMBER SIGN]\{\}\(\)\[\]\~\,\s\/]> <-[\c[APOSTROPHE]\c[QUOTATION MARK]\c[NUMBER SIGN]\{\}\(\)\[\]\~\,\s\/]>* }
  token number {
      $<sign>=[<[+\-]>?]
      [ <dec_number> | <integer> ]
  }
  token interstr { '#' <?[\c[QUOTATION MARK]]> <quote_EXPR: ':qq'> }
  token str { <?[\c[QUOTATION MARK]]> <quote_EXPR: ':q'> }

  token op { '~' | '+' | '-' | '*' | '/' }
  proto token func {*}
  rule func:sym<def> { '(' 'def' <variable> <exp> ')' }
  # (if cond then else)
  rule func:sym<if> { '(' 'if' <exp> <exp> <exp>? ')' }
  rule func:sym<fn> { '(' 'fn' <variable>? '[' ~ ']' <series> <exp> ')' }
  rule func:sym<fncall> { '(' <variable> <series> ')' }

  token series  { <exp>* }

  proto token exp {*}

  token exp:sym<vector> { '[' ~ ']' <series> }
  token exp:sym<set>  { '#{' ~ '}' <series> }
  token exp:sym<hash>  { '{' ~ '}' <series> }

  rule exp:sym<func> { <func> }
  rule exp:sym<number>  { <number>  }
  rule exp:sym<str>  { <str>  }
  rule exp:sym<variable>  { <variable>  }
  rule sexplist { <exp>* }
}

class Lilikoi::Actions is HLL::Actions {

    method TOP($/) {
        $*CUR_BLOCK.push($<stmtlist>.ast);
        make QAST::CompUnit.new( $*CUR_BLOCK );
    }

    method stmtlist($/) {
        my $stmts := QAST::Stmts.new( :node($/) );

        $stmts.push($_.ast)
            for @<stmt>;

        make $stmts;
    }

    method stmtish($/) {
        make $<modifier>
            ?? QAST::Op.new( $<EXPR>.ast, $<stmt>.ast,
                             :op(~$<modifier>), :node($/) )
            !! $<stmt>.ast;
    }

    method term:sym<value>($/) { make $<value>.ast; }

    method code-block($/) { make $<closure>.ast }

    method term:sym<call>($/) {
        my $name  := ~$<operation>;
        my $op    := %Lilikoi::Grammar::builtins{$name};

        my $call;

        if $op {
            $call := QAST::Op.new( :op($op) )
        }
        elsif %*SYM{$name} eq 'method' && $*DEF {
            $call := QAST::Op.new( :op('callmethod'),
                                   QAST::Var.new( :name('self'), :scope('lexical')),
                                   QAST::SVal.new( :value($name) ),
                );
        }
        else {
            $call := QAST::Op.new( :op('call'), :name($name) );
        }

        if $<call-args> {
            $call.push($_)
                for $<call-args>.ast;
        }

        $call.push( $<code-block>.ast )
            if $<code-block>;

        make $call;
    }

    method term:sym<super>($/) {
        my $name  := ~$*DEF;

        my $call := QAST::Op.new( :op('callmethod'),
                                  QAST::Var.new( :name('self'), :scope('lexical')),
                                  QAST::SVal.new( :value('^' ~ $name) ),
            );

        if $<call-args> {
            $call.push($_)
                for $<call-args>.ast;
        }

        $call.push( $<code-block>.ast )
            if $<code-block>;

        make $call;
    }

    method term:sym<nqp-op>($/) {
        my $op := ~$<ident>;
        my $call := QAST::Op.new( :op($op) );

        if $<call-args> {
            $call.push($_)
                for $<call-args>.ast;
        }

        make $call;
    }

    method call-args($/) {
        my @args;
        @args.push($_.ast) for $<arg>;
        make @args;
    }

    method arg:sym<expr>($/) {
        make $<EXPR>.ast
    }

    method arg:sym<func>($/) {
        make $<EXPR>.ast
    }

    method arg:sym<keyw>($/) {
        my $arg := $<EXPR>.ast;
        $arg.named( ~$<ident> );
        make $arg;
    }

    method arg:sym<hash>($/) {
        my $args := QAST::Op.new( :op<hash> );

        $args.push( $_.ast )
            for $<EXPR>;

        make $args;
    }

    method series($/) {
        make $<call-args>.ast;
    }

    method term:sym<new>($/) {

        my $tmp-obj := '$new-obj$';

        my $init-call := QAST::Op.new( :op<callmethod>,
                                       :name<initialize>,
                                       QAST::Var.new( :name($tmp-obj), :scope<lexical> )
            );

        if $<call-args> {
            $init-call.push($_)
                for $<call-args>.ast;
        }

        my $init-block := QAST::Block.new( QAST::Stmts.new(

            # pseudo-code:
            #
            # def new(*call-args)
            #     $new-obj$ = Class.new;
            #     if call-args then
            #        # always try to call initialize, when new has arguments
            #        $new-obj$.initialize(call-args)
            #     else
            #        $new-obj$.initialize() \
            #           if $new-obj$.can('initialize')
            #     end
            #     return $new-obj$
            # end

            # create the new object
            QAST::Op.new( :op('bind'),
                          QAST::Var.new( :name($tmp-obj), :scope<lexical>, :decl<var>),
                          QAST::Op.new(
                              :op('create'),
                              QAST::Var.new( :name('::' ~ ~$<ident>), :scope('lexical') )
                          ),
            ),

            # call initialize method, if available
            ($<call-args>
             ?? $init-call
             !! QAST::Op.new( :op<if>,
                              QAST::Op.new( :op<can>,
                                            QAST::Var.new( :name($tmp-obj), :scope<lexical> ),
                                            QAST::SVal.new( :value<initialize> )),
                              $init-call,
             )
            ),

            # return the new object
            QAST::Var.new( :name($tmp-obj), :scope<lexical> ),
        ));

        $init-block.blocktype('immediate');
        make $init-block;
    }

    method var($/) {
        my $sigil := ~$<sigil> // '';
        my $name  := ~$<var>;

        if $sigil eq '@' && $*IN_CLASS && $*DEF {
            # instance variable, bound to self
            my $package-name := $*CLASS_BLOCK.name;
            make QAST::Var.new( :name($name), :scope('attribute'),
                                QAST::Var.new( :name('self'), :scope('lexical')),
                                QAST::SVal.new( :value($package-name) )
                );
        }
        else {
            if $<const> {
                my $ns := $<pkg> ?? ~$<pkg> !! $*CLASS_BLOCK.name;
                $name := $ns ~ '::' ~ $<ident>;
            }

            if $*MAYBE_DECL {

                my $block;
                my $decl := 'var';

                if $sigil eq '$' || $<const> {
                    $block := $*TOP_BLOCK;
                    %*SYM-GBL{$name} := 'var';
                    %*SYM{~$<ident>} := 'var'
                        if $<const>;
                }
                elsif $sigil eq '@' {
                    $block := $*CLASS_BLOCK;
                }
                else {
                    $block := $*CUR_BLOCK;
                }

                my %sym  := $block.symbol($name);
                if !%sym<declared> {
                    %*SYM{$name} := 'var';
                    $block.symbol($name, :declared(1), :scope('lexical') );
                    my $var := QAST::Var.new( :name($name), :scope('lexical'),
                                              :decl($decl) );
                    $block[0].push($var);
                }
            }

            make QAST::Var.new( :name($name), :scope('lexical') );
        }
    }

    method term:sym<var>($/) { make $<var>.ast }

    method stmt:sym<def>($/) {
        my $install := $<defbody>.ast;
        $*CUR_BLOCK[0].push(QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($install.name), :scope('lexical'), :decl('var') ),
            $install
        ));
        if $*IN_CLASS {
            @*METHODS.push($install);
        }
        make QAST::Op.new( :op('null') );
    }

    method defbody($/) {
        $*CUR_BLOCK.name(~$<operation>);
        $*CUR_BLOCK.push($<stmtlist>.ast);
        if $*IN_CLASS {
            # it's a method, self will be automatically passed
            $*CUR_BLOCK[0].unshift(QAST::Var.new(
                :name('self'), :scope('lexical'), :decl('param')
            ));
            $*CUR_BLOCK.symbol('self', :declared(1));
        }

        make $*CUR_BLOCK;
    }

    method param($/) {
       my $var := QAST::Var.new(
           :name(~$<ident>), :scope('lexical'), :decl('param')
          );
       $var.named(~$<ident>)
           if $<named>;

       $*CUR_BLOCK.symbol('self', :declared(1));

       $var.default( $<EXPR>.ast )
          if $<EXPR>;

       make $var;
    }

    method signature($/) {
        my @params;

        @params.push($_.ast)
            for @<param>;

        if $<slurpy> {
            @params.push($<slurpy>[0].ast);
            @params[-1].slurpy(1);
        }

        if $<func> {
            @params.push($<func>[0].ast);
            @params[-1].default(QAST::Op.new( :op<null> ));
        }

        for @params {
            $*CUR_BLOCK[0].push($_) unless $_.named;
            $*CUR_BLOCK.symbol($_.name, :declared(1));
         }

         # nqp #179 named arguments need to follow positional parameters
         for @params {
            $*CUR_BLOCK[0].push($_) if $_.named;
         }
    }

    method stmt:sym<class>($/) {
        my $body_block := $<classbody>.ast;

        # Generate code to create the class.
        my $class_stmts := QAST::Stmts.new( $body_block );
        my $ins_name    := '::' ~ $<classbody><ident>;

        my $new_type :=  QAST::Op.new(
                :op('callmethod'), :name('new_type'),
                QAST::WVal.new( :value(LilikoiClassHOW) ),
                QAST::SVal.new( :value(~$<classbody><ident>), :named('name') ),
          );

        $new_type.push( QAST::SVal.new( :value(~$<classbody><super>),
                                        :named('isa') ) )
            if $<classbody><super>;

        $class_stmts.push(QAST::Op.new(
                              :op('bind'),
                              QAST::Var.new( :name($ins_name), :scope('lexical'), :decl('var') ),
                              $new_type,
                          ));

        # Add methods.
        my $class_var := QAST::Var.new( :name($ins_name), :scope('lexical') );

        for @*METHODS {
            my $name := $_.name;

            $class_stmts.push(QAST::Op.new(
                :op('callmethod'), :name('add_method'),
                QAST::Op.new( :op('how'), $class_var ),
                $class_var,
                QAST::SVal.new( :value($name) ),
                QAST::BVal.new( :value($_) ))
            );
        }

        make $class_stmts;
    }

    method classbody($/) {
        $*CUR_BLOCK.push($<stmtlist>.ast);
        $*CUR_BLOCK.blocktype('immediate');
        make $*CUR_BLOCK;
    }

    method stmt:sym<EXPR>($/) { make $<EXPR>.ast; }

    method term:sym<infix=>($/) {
        my $op := $<OPER><O><op>;
        make  QAST::Op.new( :op('bind'),
                            $<var>.ast,
                            QAST::Op.new( :op($op),
                                          $<var>.ast,
                                          $<EXPR>.ast
                            ));
    }

    method value:sym<string>($/) {
        make $<strings>.ast;
    }

    method strings($/) {
        make $<strings>
            ?? QAST::Op.new( :op('concat'), $<string>.ast, $<strings>.ast)
            !! $<string>.ast;
    }

    method string($/) {
        make $<quote_EXPR>.ast;
    }

    method value:sym<heredoc>($/) {
        make $<heredoc>.ast
    }

    method heredoc:sym<literal>($/) {
        make QAST::SVal.new( :value( ~$<text> ) );
    }

    method heredoc-line($/) { make QAST::SVal.new( :value(~$/) ) }

    method heredoc:sym<interp>($/) {
        my $value := QAST::SVal.new( :value('') );

        $value := QAST::Op.new( :op<concat>, $value, $_.ast)
            for $<text>;

        make $value;
    }

    method value:sym<integer>($/) {
        make QAST::IVal.new( :value(+$/.Str) )
    }

    method value:sym<float>($/) {
        make QAST::NVal.new( :value(+$/.Str) )
    }

    method series($/) {
        my @list;
        if $<EXPR> {
            @list.push($_.ast) for $<EXPR>
        }
        make @list;
    }

    method value:sym<array>($/) {
        my $array := QAST::Op.new( :op<list> );
        $array.push($_) for $<series>.ast;
        make $array;
    }

    method term:sym<quote-words>($/) {
        make $<quote_EXPR>.ast;
    }

    method value:sym<hash>($/) {
        my $hash := QAST::Op.new( :op<hash> );
        $hash.push($_) for $<series>.ast;
        make $hash;
    }

    method value:sym<nil>($/) {
        make QAST::Op.new( :op<null> );
    }

    method value:sym<true>($/) {
        make QAST::IVal.new( :value<1> );
    }

    method value:sym<false>($/) {
        make QAST::IVal.new( :value<0> );
    }

    method interp($/) { make $<stmtlist>.ast }
    method quote_escape:sym<#{ }>($/) { make $<interp>.ast }
    method circumfix:sym<( )>($/) { make $<EXPR>.ast }

    # todo: proper type objects
    our %call-tab;
    BEGIN {
        %call-tab := nqp::hash(
            'call', 'call',
            'nil?', 'isnull'
        )
    }

    method postfix:sym<.>($/) {
        my $op := %call-tab{ ~$<operation> };
        my $meth_call := $op
            ?? QAST::Op.new( :op($op) )
            !! QAST::Op.new( :op('callmethod'), :name(~$<operation>) );

        if $<call-args> {
            $meth_call.push($_) for $<call-args>.ast;
        }

        make $meth_call;
    }

    method postcircumfix:sym<[ ]>($/) {
        make QAST::Var.new( :scope('positional'), $<EXPR>.ast );
    }

    method postcircumfix:sym<{ }>($/) {
        make QAST::Var.new( :scope('associative'), $<EXPR>.ast );
    }

    method postcircumfix:sym<ang>($/) {
        make QAST::Var.new( :scope('associative'), $<quote_EXPR>.ast );
    }

    method xblock($/) {
        make QAST::Op.new( $<EXPR>.ast, $<stmtlist>.ast, :node($/) );
    }

    method stmt:sym<cond>($/) {
        my $ast := $<xblock>.ast;
        $ast.op( ~$<op> );
        $ast.push( $<else>.ast )
            if $<else>;

        make $ast;
    }

    method elsif($/) {
        my $ast := $<xblock>.ast;
        $ast.op( 'if' );
        $ast.push( $<else>.ast )
            if $<else>;

        make $ast;
    }

    method else($/) {
        make $<stmtlist>.ast
    }

    method stmt:sym<loop>($/) {
        make QAST::Op.new( $<EXPR>.ast, $<do-block>.ast, :op(~$<op>), :node($/) );
    }

    method stmt:sym<for>($/) {

        my $block := QAST::Block.new(
            QAST::Var.new( :name(~$<ident>), :scope('lexical'), :decl('param')),
            $<do-block>.ast,
            );

        make QAST::Op.new( $<EXPR>.ast, $block, :op('for'), :node($/) );
    }

    method do-block($/) {
        make  $<stmtlist>.ast
    }

    method term:sym<code>($/) {
        make $<stmtlist>.ast;
    }

    method closure($/) {
        $*CUR_BLOCK.push($<stmtlist>.ast);
        make QAST::Op.new(:op<takeclosure>, $*CUR_BLOCK );
    }

    method closure2($/) { self.closure($/) }

    method term:sym<lambda>($/) { make $<closure>.ast }

    method template-chunk($/) {
        my $text := QAST::Stmts.new( :node($/) );
        $text.push( QAST::Op.new( :op<print>, $_.ast ) )
            for $<template-nibble>;

        make $text;
    }

    method template-nibble:sym<interp>($/) {
        make $<interp>.ast
    }

    method template-nibble:sym<literal>($/) {
        make QAST::SVal.new( :value(~$/) );
    }

}

class Lilikoi::Compiler is HLL::Compiler {

    method eval($code, *@_args, *%adverbs) {
        my $output := self.compile($code, :compunit_ok(1), |%adverbs);

        if %adverbs<target> eq '' {
            my $outer_ctx := %adverbs<outer_ctx>;
            $output := self.backend.compunit_mainline($output);
            if nqp::defined($outer_ctx) {
                nqp::forceouterctx($output, $outer_ctx);
            }

            $output := $output();
        }

        $output;
    }
}

sub MAIN(*@ARGS) {
    my $comp := Lilikoi::Compiler.new();
    $comp.language('lilikoi');
    $comp.parsegrammar(Lilikoi::Grammar);
    $comp.parseactions(Lilikoi::Actions);
    $comp.command_line(@ARGS, :encoding('utf8'));
}
