(* Copyright 2014 INRIA *)

type dummy = Dummy
and an_option = AnOptionAttribute of Smtlib_util.pd * attribute
and attribute =
    AttributeKeyword of Smtlib_util.pd * string
  | AttributeKeywordValue of Smtlib_util.pd * string * attributevalue
and attributevalue =
    AttributeValSpecConst of Smtlib_util.pd * specconstant
  | AttributeValSymbol of Smtlib_util.pd * symbol
  | AttributeValSexpr of Smtlib_util.pd *
      attributevalsexpr_attributevalue_sexpr5
and command =
    CommandSetLogic of Smtlib_util.pd * symbol
  | CommandSetOption of Smtlib_util.pd * an_option
  | CommandSetInfo of Smtlib_util.pd * attribute
  | CommandDeclareSort of Smtlib_util.pd * symbol * string
  | CommandDefineSort of Smtlib_util.pd * symbol *
      commanddefinesort_command_symbol11 * sort
  | CommandDeclareFun of Smtlib_util.pd * symbol *
      commanddeclarefun_command_sort13 * sort
  | CommandDefineFun of Smtlib_util.pd * symbol *
      commanddefinefun_command_sortedvar15 * sort * term
  | CommandPush of Smtlib_util.pd * string
  | CommandPop of Smtlib_util.pd * string
  | CommandAssert of Smtlib_util.pd * term
  | CommandCheckSat of Smtlib_util.pd
  | CommandGetAssert of Smtlib_util.pd
  | CommandGetProof of Smtlib_util.pd
  | CommandGetUnsatCore of Smtlib_util.pd
  | CommandGetValue of Smtlib_util.pd * commandgetvalue_command_term24
  | CommandGetAssign of Smtlib_util.pd
  | CommandGetOption of Smtlib_util.pd * string
  | CommandGetInfo of Smtlib_util.pd * infoflag
  | CommandExit of Smtlib_util.pd
and commands = Commands of Smtlib_util.pd * commands_commands_command30
and identifier =
    IdSymbol of Smtlib_util.pd * symbol
  | IdUnderscoreSymNum of Smtlib_util.pd * symbol *
      idunderscoresymnum_identifier_numeral33
and infoflag = InfoFlagKeyword of Smtlib_util.pd * string
and qualidentifier =
    QualIdentifierId of Smtlib_util.pd * identifier
  | QualIdentifierAs of Smtlib_util.pd * identifier * sort
and sexpr =
    SexprSpecConst of Smtlib_util.pd * specconstant
  | SexprSymbol of Smtlib_util.pd * symbol
  | SexprKeyword of Smtlib_util.pd * string
  | SexprInParen of Smtlib_util.pd * sexprinparen_sexpr_sexpr41
and sort =
    SortIdentifier of Smtlib_util.pd * identifier
  | SortIdSortMulti of Smtlib_util.pd * identifier *
      sortidsortmulti_sort_sort44
and sortedvar = SortedVarSymSort of Smtlib_util.pd * symbol * sort
and specconstant =
    SpecConstsDec of Smtlib_util.pd * string
  | SpecConstNum of Smtlib_util.pd * string
  | SpecConstString of Smtlib_util.pd * string
  | SpecConstsHex of Smtlib_util.pd * string
  | SpecConstsBinary of Smtlib_util.pd * string
and symbol =
    Symbol of Smtlib_util.pd * string
  | SymbolWithOr of Smtlib_util.pd * string
and term =
    TermSpecConst of Smtlib_util.pd * specconstant
  | TermQualIdentifier of Smtlib_util.pd * qualidentifier
  | TermQualIdTerm of Smtlib_util.pd * qualidentifier *
      termqualidterm_term_term56
  | TermLetTerm of Smtlib_util.pd * termletterm_term_varbinding58 * term
  | TermForAllTerm of Smtlib_util.pd * termforallterm_term_sortedvar60 * term
  | TermExistsTerm of Smtlib_util.pd * termexiststerm_term_sortedvar62 * term
  | TermExclimationPt of Smtlib_util.pd * term *
      termexclimationpt_term_attribute64
and varbinding = VarBindingSymTerm of Smtlib_util.pd * symbol * term
and termexclimationpt_term_attribute64 = Smtlib_util.pd * attribute list
and termexiststerm_term_sortedvar62 = Smtlib_util.pd * sortedvar list
and termforallterm_term_sortedvar60 = Smtlib_util.pd * sortedvar list
and termletterm_term_varbinding58 = Smtlib_util.pd * varbinding list
and termqualidterm_term_term56 = Smtlib_util.pd * term list
and sortidsortmulti_sort_sort44 = Smtlib_util.pd * sort list
and sexprinparen_sexpr_sexpr41 = Smtlib_util.pd * sexpr list
and idunderscoresymnum_identifier_numeral33 = Smtlib_util.pd * string list
and commands_commands_command30 = Smtlib_util.pd * command list
and commandgetvalue_command_term24 = Smtlib_util.pd * term list
and commanddefinefun_command_sortedvar15 = Smtlib_util.pd * sortedvar list
and commanddeclarefun_command_sort13 = Smtlib_util.pd * sort list
and commanddefinesort_command_symbol11 = Smtlib_util.pd * symbol list
and attributevalsexpr_attributevalue_sexpr5 = Smtlib_util.pd * sexpr list
val dummy : unit -> unit
val pd_an_option : an_option -> Smtlib_util.pd
val pd_attribute : attribute -> Smtlib_util.pd
val pd_attributevalue : attributevalue -> Smtlib_util.pd
val pd_command : command -> Smtlib_util.pd
val pd_commands : commands -> Smtlib_util.pd
val pd_identifier : identifier -> Smtlib_util.pd
val pd_infoflag : infoflag -> Smtlib_util.pd
val pd_qualidentifier : qualidentifier -> Smtlib_util.pd
val pd_sexpr : sexpr -> Smtlib_util.pd
val pd_sort : sort -> Smtlib_util.pd
val pd_sortedvar : sortedvar -> Smtlib_util.pd
val pd_specconstant : specconstant -> Smtlib_util.pd
val pd_symbol : symbol -> Smtlib_util.pd
val pd_term : term -> Smtlib_util.pd
val pd_varbinding : varbinding -> Smtlib_util.pd
val pd_termexclimationpt_term_attribute64 : 'a * 'b list -> 'a
val pd_termexiststerm_term_sortedvar62 : 'a * 'b list -> 'a
val pd_termforallterm_term_sortedvar60 : 'a * 'b list -> 'a
val pd_termletterm_term_varbinding58 : 'a * 'b list -> 'a
val pd_termqualidterm_term_term56 : 'a * 'b list -> 'a
val pd_sortidsortmulti_sort_sort44 : 'a * 'b list -> 'a
val pd_sexprinparen_sexpr_sexpr41 : 'a * 'b list -> 'a
val pd_idunderscoresymnum_identifier_numeral33 : 'a * 'b list -> 'a
val pd_commands_commands_command30 : 'a * 'b list -> 'a
val pd_commandgetvalue_command_term24 : 'a * 'b list -> 'a
val pd_commanddefinefun_command_sortedvar15 : 'a * 'b list -> 'a
val pd_commanddeclarefun_command_sort13 : 'a * 'b list -> 'a
val pd_commanddefinesort_command_symbol11 : 'a * 'b list -> 'a
val pd_attributevalsexpr_attributevalue_sexpr5 : 'a * 'b list -> 'a
val pd : commands -> Smtlib_util.pd
