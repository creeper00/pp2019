package pp201902.project.Env
import pp201902.project.EnvIF.Bundle._
import pp201902.project.ValueIF.Bundle._

object Bundle {
  class temp (val name : String, val item : EnvItem[Env])
  type Env = List[temp]
  // implement here!
  implicit val envIF: EnvIF[Env] = new EnvIF[Env] {
    def emptyEnv(): Env = Nil

    def pushEmptyFrame(env: Env): Env = env ::: Nil

    def popFrame(env: Env): Env = env.tail

    def addItem(env: Env, name: String, item:EnvItem[Env]): Env = (new temp(name, item)) :: env

    def findItem(env: Env, name: String): (EnvItem[Env], Env) = {
      if(env.head.name == name ) (env.head.item, env)
      else findItem(env.tail, name)
    }

    def modifyEnv(env:Env, name:String, v:Val[Env]) : Unit = ???
  }

}
