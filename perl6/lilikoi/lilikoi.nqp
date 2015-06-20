use NQPHLL;

grammar Lilikoi::Grammar is HLL::Grammar {
  # Credit here goes to https://github.com/tokuhirom/Perl6-Renshu
  token TOP {
      :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
      :my $*TOP_BLOCK   := $*CUR_BLOCK;
      ^ ~ $ <sexplist>
          || <.panic('Syntax error')>
  }

  proto token value {*}
  token id { <-[\d\c[APOSTROPHE]\c[QUOTATION MARK]\c[NUMBER SIGN]\{\}\(\)\[\]\~\,\s\/]> <-[\c[APOSTROPHE]\c[QUOTATION MARK]\c[NUMBER SIGN]\{\}\(\)\[\]\~\,\s\/]>* }
  token var { <id> }
  token value:sym<var> { <var> }
  token value:sym<nil>     { <sym> }
  token value:sym<true>    { <sym> }
  token value:sym<false>   { <sym> }

  token value:sym<int> { $<sign>=[<[+\-]>?] <integer> }
  token value:sym<dec> { $<sign>=[<[+\-]>?] <dec_number> }
  token value:sym<string> { <interstr> | <str> }
  token interstr { '#' <?[\c[QUOTATION MARK]]> <quote_EXPR: ':qq'> }
  token str { <?[\c[QUOTATION MARK]]> <quote_EXPR: ':q'> }

  proto rule func {*}
  rule func:sym<def> {
    :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
    :my %sym-save := nqp::clone(%*SYM);
    :my $*DEF;
    '(' 'def' <id> ~ ')' <defbody> {
        %sym-save{$*DEF} := %*SYM{$*DEF};
        %*SYM := %sym-save;
    }
  }

  rule defbody {
      <id> {
          $*DEF := ~$<id>;
          %*SYM{$*DEF} := 'func';
      }
      <exp>
  }

  rule func:sym<fn> {
    :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
    '(' 'fn' ~ ')' <fnbody>
  }

  rule fnbody {
      <id>? {
          %*SYM{~$<id>} := 'func'
      }
      [ '[' ~ ']' <signature>? ]
      <sexplist>
  }

  rule signature {
      [ <param> | '&' <slurpy=.param> ] +
  }

  token param {
      <id> {
          %*SYM{~$<id>} := 'var'
      }
  }

  # (if cond then else)
  rule func:sym<if> {
    :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
    '(' 'if' <exp> <exp> <exp>? ')'
  }

  rule func:sym<call> {
    :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
    '(' <var> ~ ')' <call-args=.series>
  }

  rule series  { <exp>* }

  rule vector { '[' ~ ']' <series> }
  rule hashmap { '{' ~ '}' <series> }

  proto token comment {*}
  token comment:sym<line>   { ';' [ \N* ] }
  token comment:sym<discard> { '#_' <.exp> }

  token ws { <!after <variable>> <!before <variable>> [\s | ',' | <.comment> ]* }

  proto rule exp {*}

  rule exp:sym<vector> { <vector> }
  rule exp:sym<set>  { '#{' ~ '}' <series> }
  rule exp:sym<hash>  { <hashmap> }

  rule exp:sym<func> { <func> }
  rule exp:sym<value> { <value> }

  rule sexplist { <exp>* }
}

class Lilikoi::Actions is HLL::Actions {

  method TOP($/) {
      $*CUR_BLOCK.push($<sexplist>.ast);
      make QAST::CompUnit.new( $*CUR_BLOCK );
  }
  method sexplist($/) {
      my $stmts := QAST::Stmts.new( :node($/) );

      if $<exp> {
          for $<exp> {
              $stmts.push($_.ast)
          }
      }

      make $stmts;
  }


  method exp:sym<value>($/) { make $<value>.ast; }

  method value:sym<int>($/) {
    make QAST::IVal.new( :value(+$/.Str) )
  }

  method value:sym<dec>($/) {
    make QAST::NVal.new( :value(+$/.Str) )
  }

  method value:sym<string>($/) {
    make $<quote_EXPR>.ast;
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

  method var($/) {
    my $name  := ~$<var>;

    make QAST::Var.new( :name($name), :scope('lexical') );
  }

  method func:sym<def>($/) {
    my $name  := ~$<var>;
    my $block := $*CUR_BLOCK;
    my $decl := 'var';

    my %sym  := $block.symbol($name);
    if !%sym<declared> {
        %*SYM{$name} := 'var';
        $block.symbol($name, :declared(1), :scope('lexical') );
        my $var := QAST::Var.new( :name($name), :scope('lexical'),
                                  :decl($decl) );
        $block[0].push($var);
    }
    make QAST::Var.new( :name($name), :scope('lexical') );
  }

  method value:sym<var>($/) { make $<var>.ast }

  method closure($/) {
    $*CUR_BLOCK.name(~$<id>);
    $*CUR_BLOCK.push($<sexplist>.ast);
    make QAST::Op.new(:op<takeclosure>, $*CUR_BLOCK );
  }

  method func:sym<fn>($/) { make $<closure>.ast }

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

      for @params {
          $*CUR_BLOCK[0].push($_) unless $_.named;
          $*CUR_BLOCK.symbol($_.name, :declared(1));
      }
  }



    method code-block($/) { make $<closure>.ast }

    method func:sym<call>($/) {
        my $name  := ~$<operation>;

        my $call := QAST::Op.new( :op('call'), :name($name) );

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

    method term:sym<lambda>($/) { make $<closure>.ast }

    method closure2($/) { self.closure($/) }


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
