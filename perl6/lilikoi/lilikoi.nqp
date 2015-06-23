use NQPHLL;
use QRegex;

grammar Lilikoi::Grammar is HLL::Grammar {
  # Credit here goes to https://github.com/tokuhirom/Perl6-Renshu
  method TOP() {
      my %*LANG;
      %*LANG<Regex>         := NQP::Regex;
      %*LANG<Regex-actions> := NQP::RegexActions;
      %*LANG<MAIN>          := Lilikoi::Grammar;
      %*LANG<MAIN-actions>  := Lilikoi::Actions;
      my $file := nqp::getlexdyn('$?FILES');
      my $source_id := nqp::sha1(self.target()) ~
          (%*COMPILING<%?OPTIONS><stable-sc> ?? '' !! '-' ~ ~nqp::time_n());
      my $*W := nqp::isnull($file) ??
          NQP::World.new(:handle($source_id)) !!
          NQP::World.new(:handle($source_id), :description($file));
      my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
      my $*TOP_BLOCK   := $*CUR_BLOCK;
      self.comp_unit;
  }
  token comp_unit {
    <.outerctx>
    <sexplist>
    [ $ || <.panic: 'Syntax error'> ]

  }

  proto token value {*}
  token id {
    <-[\d\c[APOSTROPHE]\c[QUOTATION MARK]\c[NUMBER SIGN]\{\}\(\)\[\]\s\/`@,~\:\^\.]>
    <-[\c[APOSTROPHE]\c[QUOTATION MARK]\c[NUMBER SIGN]\{\}\(\)\[\]\s\/`@,~\:\^\.]>*
  }
  token var { <id> }
  token declare { <id> }
  token value:sym<var> { <var> }
  token value:sym<nil>     { <sym> }
  token value:sym<true>    { <sym> }
  token value:sym<false>   { <sym> }

  token value:sym<int> { $<sign>=[<[+\-]>?] <integer> }
  token value:sym<dec> { $<sign>=[<[+\-]>?] <dec_number> }
  token value:sym<string> { <interstr> | <str> }

  token interstr { '#' <?[\c[QUOTATION MARK]]> <quote_EXPR: ':qq'> }
  token str { <?[\c[QUOTATION MARK]]> <quote_EXPR: ':q'> }

  token newpad { <?> }
  token outerctx { <?> }

  token value:sym</ />  {
      '/'
      <.newpad>
      :my %*RX;
      <p6regex=.LANG('Regex','nibbler')>
      '/'
  }

  proto token func {*}
  rule func:sym<def> {
    :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
    '('
    'def'
    <declare> {
        %*SYM{~$<declare>} := 'var'
    }
    <exp>
    ')'
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

  rule func:sym<let> {
    :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
    '(' 'let' ~ ')' <letbody>
  }

  rule letbody {
      [ '[' ~ ']' <bindings> ]
      <sexplist>
  }

  rule bindings {
      [ <bindpair> ] +
  }

  token bindpair {
      <declare> {
          %*SYM{~$<declare>} := 'var'
      }
      <exp>
  }

  # (if cond then else)
  rule func:sym<if> {
    :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
    '(' 'if' <exp> <exp> <exp>? ')'
  }

  rule func:sym<call> {
    :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
    '(' <var> ~ ')' <series>
  }

  rule func:sym<nqp-op> {
    :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
    '(' '.' <id> ~ ')'  <series>
  }

  rule func:sym<while> {
    :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
    '(' 'while' <exp> ~ ')' <series>
  }

  rule func:sym<foreach> {
    :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
    '('
    'foreach'
    '['
    <id>  {
        %*SYM{~$<id>} := 'var'
    }
    <exp>
    ']'
    ~
    ')'
    <series>
  }
  rule series  { [ <exp> ] * }

  proto token comment {*}
  token comment:sym<line>   { ';' [ \N* ] }
  token comment:sym<discard> { '#_' <.exp> }

  #token ws { <!after <.id> > <!before <.id> > [ \s | ',' | <.comment> ]* }
  token ws {
    [ \s
    | ','
    | <.comment>
    ]* }

  proto token exp {*}

  rule value:sym<vector> { '[' ~ ']' <series> }
  rule value:sym<set>  { '#{' ~ '}' <series> }
  rule value:sym<hash>  { '{' ~ '}' <series> }

  rule exp:sym<func> { <func> }
  rule exp:sym<value> { <value> }

  rule sexplist { <exp>* }
}

class Lilikoi::Actions is HLL::Actions {

  method comp_unit($/) {
      $*CUR_BLOCK.push($<sexplist>.ast);
      make QAST::CompUnit.new( $*CUR_BLOCK );
  }

  method sexplist($/) {
      my $stmts := QAST::Stmts.new( :node($/) );

      if $<exp> {
          $stmts.push($_.ast)
            for @<exp>;
      }

      make $stmts;
  }

  method exp:sym<value>($/) { make $<value>.ast; }

  method exp:sym<func>($/) {
      make $<func>.ast;
  }

  method value:sym<int>($/) {
    my $value := $<integer>.ast;
    if ~$<sign> eq '-' { $value := -$value; }
    make QAST::IVal.new( :value(+$/.Str) )
  }

  method value:sym<dec>($/) {
    my $value := $<dec_number>.ast;
    if ~$<sign> eq '-' { $value := -$value; }
    make QAST::NVal.new( :value(+$/.Str) )
  }

  method value:sym<string>($/) {
    make $<interstr>
        ?? $<interstr>.ast
        !! $<str>.ast;
  }

  method str($/) {
    make $<quote_EXPR>.ast;
  }

  method interstr($/) {
    make $<quote_EXPR>.ast;
  }

  method newpad($/) {
      $*W.push_lexpad($/)
  }

  method value:sym</ />($/) {
      my $block := $*W.pop_lexpad();
      $block[0].push(QAST::Var.new(:name<self>, :scope<lexical>, :decl<param>));
      $block[0].push(QAST::Op.new(
          :op('bind'),
          QAST::Var.new(:name<self>, :scope<local>, :decl('var') ),
          QAST::Var.new( :name<self>, :scope('lexical') )));
      $block[0].push(QAST::Var.new(:name<$¢>, :scope<lexical>, :decl('var')));
      $block[0].push(QAST::Var.new(:name<$/>, :scope<lexical>, :decl('var')));
      $block.symbol('$¢', :scope<lexical>);
      $block.symbol('$/', :scope<lexical>);

      my $regex := %*LANG<Regex-actions>.qbuildsub($<p6regex>.ast, $block);
      my $ast := QAST::Op.new(
          :op<callmethod>, :name<new>,
          lexical_package_lookup(['NQPRegex'], $/),
          $regex);

      # In sink context, we don't need the Regex::Regex object.
      $ast.annotate('sink', $regex);
      make $ast;
  }

  method id($/) {
      make ~$/.ast;
  }

  method var($/) {
    make QAST::Var.new( :name(~$<id>), :scope('lexical') );
  }

  method declare($/) {
    my $name  := ~$<id>;
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

  method func:sym<def>($/) {
    make QAST::Op.new(
            :op<bind>,
            $<declare>.ast,
            $<exp>.ast
    );
  }

  method value:sym<var>($/) { make $<var>.ast }

  method func:sym<fn>($/) { make $<fnbody>.ast }

  method func:sym<fnbody>($/) {
    if $<id> {
      $*CUR_BLOCK.name(~$<id>);
    }
    $*CUR_BLOCK.push($<sexplist>.ast);
    make QAST::Op.new(:op<takeclosure>, $*CUR_BLOCK );
  }

  method param($/) {
    my $var := QAST::Var.new(
      :name(~$<id>), :scope('lexical'), :decl('param')
    );

    # $*CUR_BLOCK.symbol('self', :declared(1)); # not sure if needed

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

  method func:sym<let>($/) { make $<letbody>.ast }

  method func:sym<letbody>($/) {
    make $<sexplist>.ast;
  }
  method bindings($/) {
      my @bindpairs;

      @bindpairs.push($_.ast)
          for @<bindpair>;
  }

  method bindpair($/) {
    make QAST::Op.new(
            :op<bind>,
            $<declare>.ast,
            $<exp>.ast
    );
  }

  method func:sym<call>($/) {
      my $name  := ~$<var>;

      my $call := QAST::Op.new( :op('call'), :name($name) );

      if $<series> {
          $call.push($_)
              for @<series>.ast;
      }

      make $call;
  }

  method func:sym<nqp-op>($/) {
      my $op := ~$<id>;
      my $call := QAST::Op.new( :op($op) );

      if $<series> {
          $call.push($_)
              for @<series>.ast;
      }

      make $call;
  }
  # also credited to https://github.com/tokuhirom/Perl6-Renshu
  method func:sym<if>($/) {
    my $op := QAST::Op.new(
        :op<if>,
        block_immediate(QAST::Block.new($<exp>[0].ast)),
        block_immediate(QAST::Block.new($<exp>[1].ast)),
        :node($/)
    );
    if nqp::elems($<exp>) == 3 {
        $op.push(block_immediate(QAST::Block.new($<exp>[2].ast)));
    }
    make $op;
  }
  # Ref. src/NQP/Actions.nqp
  sub block_immediate($block) {
    $block.blocktype('immediate');
    unless $block.symtable() {
        my $stmts := QAST::Stmts.new( :node($block.node) );
        for $block.list { $stmts.push($_); }
        $block := $stmts;
    }
    $block;
  }

  method series($/) {
      my @list;
      if $<exp> {
          @list.push($_.ast) for @<exp>
      }
      make @list;
  }

  method value:sym<vector>($/) {
      my $vector := QAST::Op.new( :op<list> );
      $vector.push($_) for $<series>.ast;
      make $vector;
  }

  method value:sym<hash>($/) {
      my $hash := QAST::Op.new( :op<hash> );
      $hash.push($_) for $<series>.ast;
      make $hash;
  }

  method value:sym<set>($/) {
      my $hash := QAST::Op.new( :op<hash> );
      for $<series>.ast {
        $hash.push($_);
        $hash.push(QAST::IVal.new( :value<1> ).ast);
      }
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




  method func:sym<while>($/) {
      make QAST::Op.new( $<exp>.ast, $<series>.ast, :op('while'), :node($/) );
  }

  method func:sym<foreach>($/) {
    my $block := QAST::Block.new(
      QAST::Var.new( :name(~$<id>), :scope('lexical'), :decl('param')),
      $<series>.ast,
    );

    make QAST::Op.new( $<exp>.ast, $block, :op('for'), :node($/) );
  }

}

class Lilikoi::Compiler is HLL::Compiler {

    method eval($code, *@_args, *%adverbs) {
        say($code);
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
