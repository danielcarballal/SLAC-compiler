package slacc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._
  import scala.collection.immutable.List

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._
    var current: Token = new Token(BAD) //The first one is bad for no particular reason
    var currentChar: Char = source.next; //Start with a blank for the same reason
    var index: Int  = 0

    def curChar(): Char = {
      currentChar
    }
    def nextChar(): Char = {
      currentChar = source.next
      // println("Position updated to " + source.pos)
      currentChar
    }

    // Complete this file

    new Iterator[Token] {

      def hasNext = {
        source.hasNext
      }

      def next = {
        // println("Cur char is " + curChar)
        var position = source.pos; //Read before so that position is always start of literal
        if(!hasNext){
          current = new Token(EOF);
        }
        else if(curChar().isLetter)
        {
          var buf: String = ""
          while((curChar().isLetter || curChar().isDigit))
          {
            buf += curChar();
            nextChar();
          }
            buf match{
              case "class" =>
                current = new Token(CLASS);
              case "method" =>
                current = new Token(METHOD);
              case "var" =>
                current = new Token(VAR);
              case "Unit" =>
                current = new Token(UNIT);
              case "String" =>
                current = new Token(STRING);
              case "Int" =>
                current = new Token(INT);
              case "Boolean" =>
                current = new Token(BOOLEAN);
              case "While" =>
                current = new Token(WHILE);
              case "if" =>
                current = new Token(IF);
              case "else" =>
                current = new Token(ELSE);
              case "length" =>
                current = new Token(LENGTH);
              case "true" =>
                current = new Token(TRUE);
              case "false" =>
                current = new Token(FALSE);
              case "self" =>
                current = new Token(SELF);
              case "New" =>
                current = new Token(NEW);
              case "println" =>
                current = new Token(PRINTLN);
              case "strOf" =>
                current = new Token(STROF);
              case _ =>
                current = new ID(buf);
              }
        }
        else if(curChar().isDigit){
          var value: Int = curChar.toInt - '0'.toInt;
          while(nextChar.isDigit){
            println("Value is " + value);
            value = value * 10
            value = value + curChar.toInt - '0'.toInt;
          }
          current = new INTLIT(value);
        }
        else curChar() match { //Current 
          case '(' => nextChar(); current=new Token(LPAREN);
          case ')' => nextChar(); current=new Token(RPAREN);
          case ':' => nextChar(); current=new Token(COLON);
          case ';' => nextChar(); current=new Token(SEMICOLON);
          case '.' => nextChar(); current=new Token(DOT);
          case ',' => nextChar(); current=new Token(COMMA);
          case '!' => nextChar(); current=new Token(BANG);
          case '[' => nextChar(); current=new Token(LBRACKET);
          case ']' => nextChar(); current=new Token(RBRACKET);
          case '{' => nextChar(); current=new Token(LBRACE);
          case '}' => nextChar(); current=new Token(RBRACE);
          case '<' => nextChar(); current=new Token(LESSTHAN);
          case '+' => nextChar(); current=new Token(PLUS);
          case '-' => nextChar(); current=new Token(MINUS);
          case '*' => nextChar(); current=new Token(TIMES);
          case '/' => 
            nextChar();
            if(curChar() == '/'){
              while(curChar() != '\n')
              {
                if(!hasNext){ current = new Token(EOF);} //Need to break here?
                nextChar();
              }
              next;
            }
            else if(curChar() == '*')
            {
              var found: Boolean = false;
              while(!found){
                while(curChar() != '*'){
                  if(!hasNext){ current = new Token(EOF);}
                  nextChar();
                }
                if(nextChar()== '/'){ found = true; nextChar() }
              }
              next
            }
            else{
              current=new Token(DIV);
            }
          case '=' => 
            nextChar()
            if(curChar() == '='){ nextChar(); current = new Token(EQUALS); }
            else {current = new Token(EQSIGN); }
          case '&' =>
            nextChar()
            if(curChar() == '&'){ nextChar(); current = new Token(AND); }
            else {current = new Token(BAD);} // Can't have single &
          case '|' =>
            nextChar()
            if(curChar() == '|'){ nextChar(); current = new Token(OR); }
            else {current = new Token(BAD);} // Can't have single |
          case '\n' =>
            nextChar; next;
          case ' ' =>
            nextChar; next;
          case _ => println("UNEXPECTED CHARECTER " + curChar); nextChar(); next;
        }
        current.setPos(f, position)
        current

      }
    }

  }
}

object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    new Iterator[Token] {
      def hasNext = {
        tokens.hasNext
      }

      def next = {
        val n = tokens.next
        println(n+"("+n.line+":"+n.col+") ")
        n
      }
    }

  }
}
