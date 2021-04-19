package pp201902.project.Interpreter
import pp201902.project.EnvIF.Bundle._
import pp201902.project.ExpressionIF.Bundle._
import pp201902.project.InterpreterIF.Bundle._
import pp201902.project.ValueIF.Bundle._


object Bundle {
  implicit def interpreterIF[Env](implicit ENV: EnvIF[Env]) : InterpreterIF[Env]  = new InterpreterIF[Env] {
    implicit val env = ENV.emptyEnv()
    // implement here!
    def interp(expr: Expr): Val[Env] = {
      def interpp(expr: Expr)(implicit env : Env) : Val[Env] = {
        expr match {
          case EName(a) => {
            ENV.findItem(env, a) match {
            case (EVal(v),_) => v
            case (EDef(p,e),_) => p match {
              case ANname(null)::Nil => interpp(e)(env)
              case _ => VDef(p,e,env)
            }
            case (ELval(e),_) => interpp(e)(env)
          }}
          case EInt(n) => VInt(n)
          case ETrue() => VBool(true)
          case EFalse() => VBool(false)
          case ENil() => VNil()
          case EIf(econd, et, ef) => if(interpp(econd)(env) == VBool(true)) interpp(et)(env)
          else  interpp(ef)(env)
          case ECons(a,b) => VCons(interpp(a)(env), interpp(b)(env))
          case EFst( ECons(a,b)) => interpp(a)(env)
          case ESnd( ECons(a,b)) => interpp(b)(env)
          case EIsNil(e) => if(interpp(e)(env) == VNil()) VBool(true)
          else VBool(false)
          case EPlus(a, b) => {
            val c = interpp(a)(env)
            val d = interpp(b)(env)
            (c, d) match {
              case (VInt(x), VInt(y)) => VInt(x+y)
              }
          }
          case EMinus(a,b) => {
            val c = interpp(a)(env)
            val d = interpp(b)(env)
            (c, d) match {
              case (VInt(x), VInt(y)) => VInt(x-y)
            }
          }
          case EMult(a,b) => {
            val c = interpp(a)(env)
            val d = interpp(b)(env)
            (c, d) match {
              case (VInt(x), VInt(y)) => VInt(x*y)
            }
          }
          case EEq(a,b) => {
            val c = interpp(a)(env)
            val d = interpp(b)(env)
            (c, d) match {
              case (VInt(x), VInt(y)) => if(x==y) VBool(true)
              else VBool(false)
            }
          }
          case ELt(a,b) => {
            val c = interpp(a)(env)
            val d = interpp(b)(env)
            (c, d) match {
              case (VInt(x), VInt(y)) => if(x<y) VBool(true)
              else VBool(false)
            }
          }
          case EGt(a,b) => {
            val c = interpp(a)(env)
            val d = interpp(b)(env)
            (c, d) match {
              case (VInt(x), VInt(y)) => if(x>y) VBool(true)
              else VBool(false)
            }
          }
          case ELet(bs, eb) => {
            val count = bs.size
            def frameadd(i : Int, e : Env): Env = {
              if(i==count) e
              else bs(i) match {
                case BVal(a,b) => frameadd(i+1, ENV.addItem(e, a, EVal(interpp(b)(e))))
                case BDef(f,p,x) => {
                  frameadd(i+1, ENV.addItem(e, f, EDef(p,x)))
                }
                case BLval(a,b) => frameadd(i+1, ENV.addItem(e, a, ELval(b)))
              }
            }
            val v = frameadd(0,env)
            interpp(eb)(v)
          }
          case EApp(ef, ergs)  => {
            val c = interpp(ef)(env)
            c match {
              case VDef(p,b,e) => {
                def maker(l1 : List[Arg], l0 : List[Expr], l3 : List[(EnvItem[Env], String)]) : List[(EnvItem[Env], String)] = {
                  l1 match {
                    case Nil => l3
                    case AVname(a)::tail => maker(tail, l0.tail, (EVal(interpp(l0.head)(e)),a)::l3)
                    case ANname(a)::tail => maker(tail, l0.tail, ((EDef[Env](ANname(null)::Nil, l0.head)),a)::l3)
                  }
                }
                def applyer (l1 : List[Arg], l2 : List[(EnvItem[Env],String)], en : Env): Env = {
                  if(l1 ==Nil ) en
                  else {
                    l1.head match {
                      case AVname(x) => {
                        applyer(l1.tail,l2.tail, ENV.addItem(en, x, l2.head._1))
                      }
                      case ANname(x) => {
                        applyer(l1.tail,l2.tail, ENV.addItem(en, x, l2.head._1))
                      }
                    }
                  }
                }
                interpp(b)(applyer(p,maker(p,ergs,Nil),e))
              }
            }
          }
          case ERmk(bs) => {
            val count = bs.size
            def frameadder(i : Int , eu : Env) : Env = {
              if(i == count) eu
              else bs(i) match {
                case BVal(a,b) => frameadder(i+1, ENV.addItem(eu, a, EVal(interpp(b)(eu))))
                case BDef(f,p,x) => {
                  frameadder(i+1, ENV.addItem(eu, f, EDef(p,x)))
                }
                case BLval(a,b) => frameadder(i+1, ENV.addItem(eu, a, ELval(b)))
              }
            }
            VRec(frameadder(0,env),env)
          }
          case ERfd(rec, fd) => {
            val re = interpp(rec)(env)
            re match {
              case VRec(fields, env) => ENV.findItem(fields,fd) match {
                case (EVal(v),_) => v
                case (EDef(p,e),_) => if(p!=Nil) VDef(p,e,env)
                                      else interpp(e)(env)
                case (ELval(e),_) => interpp(e)(env)
              }
            }
          }
        }}
      interpp(expr)
    }
  }
}
