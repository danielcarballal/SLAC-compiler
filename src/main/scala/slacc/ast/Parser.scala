package slacc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next
        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def skip(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    def parseGoal: Program = {

      var classlist: List[ClassDecl] = Nil
      while(currentToken.kind == CLASS){ classlist = classlist :+ classdefinition; }
      var mainmeth = mainmethoddelceration
      var prog = Program(mainmeth, classlist)
      prog
    }

    def classdefinition: ClassDecl ={
      //class Identifier ( <: Identifier )? { ( VarDeclaration )* ( MethodDeclaration )* }
      //case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl])
      skip(CLASS)
      var id = identifier 
      var parent: Option[Identifier] = None
      if(currentToken.kind == LESSTHAN){
        skip(LESSTHAN)
        skip(COLON)
        parent = Some(identifier)
      }
      skip(LBRACE)
      var varList: List[VarDecl] = Nil
      while(currentToken.kind == VAR){ var temp = vardecleration; varList = varList :+ temp }

      var methList: List[MethodDecl] = Nil
      while(currentToken.kind == METHOD){var m = methoddecleration; methList = methList :+ m; }
      skip(RBRACE)
      ClassDecl(id, parent, varList, methList );
    }

    def vardecleration: VarDecl = {
      //var Identifier : Type ;
      skip(VAR)
      var id = identifier
      skip(COLON)
      var t = typedeclaration
      skip(SEMICOLON)

      VarDecl(t, id)
    }

    def mainmethoddelceration: MainMethod = {
      //method Identifier ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type = { ( VarDeclaration )* Expression ( ; Expression )* }
      var md = methoddecleration
      MainMethod(md)

    }

    //method Identifier ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type = { ( VarDeclaration )* Expression ( ; Expression )* }
    //MethodDecl(retType: TypeTree, id: Identifier, args: List[Formal], vars: List[VarDecl], exprs: List[ExprTree], retExpr: ExprTree)
    def methoddecleration: MethodDecl = {
      skip(METHOD)
      var methname = identifier
      skip(LPAREN)
      var args: List[Formal] = Nil
      if(currentToken.kind == IDKIND){
        var firstId: Identifier = identifier
        skip(COLON)
        var firstType: TypeTree = typedeclaration
        args = args :+ Formal(firstType, firstId)
        while(currentToken.kind == COMMA){
          readToken
          var id: Identifier = identifier
          skip(COLON)
          var stype: TypeTree = typedeclaration
          args = args :+ Formal(stype, id)
        }
      }
      skip(RPAREN)
      skip(COLON)
      var retType = typedeclaration
      skip(EQSIGN)

      skip(LBRACE)

      /* TODO FIX FIX */
      var varList: List[VarDecl] = Nil
      while(currentToken.kind == VAR) {varList = varList :+ vardecleration;}
      var latestExpr: ExprTree = expr 
      var exprList: List[ExprTree] = Nil
      while(currentToken.kind == SEMICOLON) {
        skip(SEMICOLON);
        exprList = exprList :+ latestExpr;
        latestExpr = expr;
      }
      skip(RBRACE)

      MethodDecl(retType, methname, args, varList, exprList, latestExpr)
    }

    def typedeclaration: TypeTree = {
      var typeRet: TypeTree = new BooleanType(); //Placeholder

      currentToken.kind match{
       case BOOLEAN =>
        skip(BOOLEAN)
        typeRet = BooleanType()
       case STRING => 
        skip(STRING)
        typeRet =  StringType()
       case UNIT =>
        skip(UNIT)
        typeRet =  UnitType();
       case INT =>
          skip(INT)
          if(currentToken.kind == LBRACKET){
            skip(LBRACKET)
            skip(RBRACKET)
            typeRet =  IntArrayType()
          } else {
            typeRet = IntType()
          }
        case IDKIND =>
          typeRet = identifier
        case _ =>
          fatal(currentToken + " is not a valid type!")
      }
      typeRet
    }



    def expr: ExprTree = {
      var ret: ExprTree = expr2
      while(currentToken.kind == OR){
        skip(OR)
        var exprnew: ExprTree = expr2
        ret = Or(ret, exprnew)
      }
      ret
    }

    def expr2: ExprTree = {
      var ret : ExprTree = expr3
      while(currentToken.kind == AND){
        skip(AND)
        var exprnew: ExprTree = expr3
        ret = And(ret, exprnew)
      }
      ret 
    }

    def expr3: ExprTree = {
      /* Comparison operators */
      var ret : ExprTree = expr4
      while(currentToken.kind == LESSTHAN || currentToken.kind == EQUALS){
        if(currentToken.kind == LESSTHAN){
          skip(LESSTHAN)
          var exprnew: ExprTree = expr4
          ret = LessThan(ret, exprnew)
        }
        if(currentToken.kind == EQUALS){
          skip(EQUALS)
          var exprnew: ExprTree = expr4
          ret = Equals(ret, exprnew)
        }
      }
      ret 
    }

    def expr4 : ExprTree = {
      var ret : ExprTree = expr5
      while(currentToken.kind == PLUS || currentToken.kind == MINUS){
        if(currentToken.kind == PLUS){
          skip(PLUS)
          var exprnew: ExprTree = expr5
          ret = Plus(ret, exprnew)
        }
        if(currentToken.kind == MINUS){
          skip(MINUS)
          var exprnew: ExprTree = expr5
          ret = Minus(ret, exprnew)
        }
      }
      ret 
    }

    def expr5 : ExprTree = {
      var ret : ExprTree = simpleexpr
      while(currentToken.kind == TIMES || currentToken.kind == DIV){
        if(currentToken.kind == TIMES){
          skip(TIMES)
          var exprnew: ExprTree = simpleexpr
          ret = Times(ret, exprnew)
        }
        if(currentToken.kind == DIV){
          skip(DIV)
          var exprnew: ExprTree = simpleexpr
          ret = Div(ret, exprnew)
        }
      }
      ret
    }

    def simpleexpr: ExprTree = {
      var retVal: ExprTree = True() //placeholder
      currentToken.kind match {
        case INTLITKIND =>
          retVal = IntLit(getInt(currentToken))
          skip(INTLITKIND)
        case STRLITKIND =>
          retVal = StringLit(getString(currentToken))
          skip(STRLITKIND)
        case TRUE =>
          skip(TRUE)
          retVal = True()
        case FALSE =>
          skip(FALSE)
          retVal = False()
        case SELF =>
          skip(SELF)
          retVal = Self()
        case LPAREN =>
          skip(LPAREN)
          retVal = expr
          skip(RPAREN)

        case NEW =>
          skip(NEW)
          if(currentToken.kind == INT){
            skip(INT)
            skip(LBRACKET)
            var ex = expr
            skip(RBRACKET)
            retVal = NewIntArray(ex)
          } else if(currentToken.kind == IDKIND){
            var id = identifier
            skip(LPAREN); skip(RPAREN)
            retVal = New(id)
          }

        case IF =>
          skip(IF)
          skip(LPAREN)
          var condition : ExprTree = expr
          skip(RPAREN)
          var block : ExprTree = expr //Might be block or expression?
          var optionalElse : Option[ExprTree] = None
          if(currentToken.kind == ELSE){
            skip(ELSE)
            optionalElse = Some(expr)
          }
          retVal = If(condition, block, optionalElse)

        case LBRACE =>
          skip(LBRACE)
          var listExpr: List[ExprTree] = Nil
          listExpr = listExpr :+ expr
          while(currentToken.kind == SEMICOLON){ skip(SEMICOLON); listExpr = listExpr :+ expr }
          skip(RBRACE)

          retVal = Block(listExpr)

        case WHILE =>
          skip(WHILE)
          skip(LPAREN)
          var condition: ExprTree = expr
          skip(RPAREN)
          var block = expr
          retVal = While(condition, block)

        case PRINTLN =>
          skip(PRINTLN)
          skip(LPAREN)
          var newexpr = expr
          skip(RPAREN)
          retVal = Println(newexpr)

        case STROF =>
          skip(STROF)
          skip(LPAREN)
          var newexpr : ExprTree = expr
          skip(RPAREN)
          retVal = Strof(newexpr)

        case BANG =>
          skip(BANG)
          var exprnew: ExprTree = simpleexpr
          retVal = Not(exprnew)
        case IDKIND =>
          var id = identifier
          if(currentToken.kind == EQSIGN){
            skip(EQSIGN)
            var newexpr = expr
            retVal = Assign(id, newexpr)
          } else if(currentToken.kind == LBRACKET){
            skip(LBRACKET)
            var index = expr
            skip(RBRACKET)
            if(currentToken.kind == EQSIGN)
            {
              skip(EQSIGN) 
              var set = expr
              retVal = ArrayAssign(id, index, set)
            }
            else{
              retVal = ArrayRead(id, index)
            }
          } else{ //Is simply ID
            retVal = id
          }
          case _ =>
            fatal("Expected expression, instead got " + currentToken.toString)
      }
      retVal = exprfollow(retVal)
      retVal
    }

    def exprfollow(exprIn : ExprTree) : ExprTree = {
        var ret : ExprTree = exprIn
        while(currentToken.kind == LBRACKET){
          skip(LBRACKET)
          var exprnew: ExprTree = expr
          skip(RBRACKET)
          ret = exprfollow(ArrayRead(exprIn, exprnew))
        }
        while(currentToken.kind == DOT){
          skip(DOT)
          if(currentToken.kind == LENGTH){
            skip(LENGTH)
            ret = exprfollow(ArrayLength(exprIn))
          }
          else{
            var id = identifier
            skip(LPAREN)
            var exprList: List[ExprTree] = Nil
            if(currentToken.kind != RPAREN){ //At least one arg
              exprList = exprList :+ expr
              while(currentToken.kind == COMMA){
                skip(COMMA)
                exprList = exprList :+ expr
              }
            }
    
            skip(RPAREN)
            ret = exprfollow(MethodCall(ret, id, exprList))
          }
        }
        ret
    }

    def identifier: Identifier = {
      var id = Identifier(getString(currentToken))
      skip(IDKIND)
      id
    }

    def getString(input: Token): String = {
      input match {
        case id: ID => id.value
        case string: STRLIT => string.value
        case _ => fatal("expected something that had a string value (probably building an ID), was " + currentToken.toString)
      }
    }

    def getInt(input: Token): Integer = {
      input match {
        case number: INTLIT => number.value
        case _ => fatal("expected something that had a number value")
      }
    }


    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
