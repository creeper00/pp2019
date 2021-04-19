# E language Interpreter (Scala)

## E Language Definition

![image](https://user-images.githubusercontent.com/43807159/115174189-c1016500-a103-11eb-8be7-3020ddde1cc0.png)

## Implementation Specs

1.
- For ill-typed inputs, you can return arbitrary values, or raise exceptions.  
- X* denotes that X can appear 0 or more times.  
- let clauses create a new scope like a `block' in Scala. Name bindings def, and val work the similar way as in Scala. 
  - { (def f (A*) E) assigns name f to expression E with arguments  
  - A*. Examples include (def f (a b) (+ a b)) and (def g () 3).  
  - { (val x E) assigns name x to the value obtained by evaluating E.  
  - { We do not allow the same name to be defined twice in the frame.  
  - { You do not have to consider forward reference in val. For example, (val x (cons 1 x)).  
  - { Hint: Implement environment with mutable data structure for lazyness.  
- Enviornment is collection of Frames. Frame is created when a new scope is created.  
- (nil? E) first evaluates E into value v. If v is nil, it returns true. Otherwise, it returns false.  
- For additional information, post questions on the GitHub course webpage.  
- examples in src/test/scala/TestMain.scala.  

2.
- Optimize interp to handle tail recursive input programs  
 
3. 
- Add lazy evaluation to interp by implementing by-name and lazy-val  

4.
- Add record to interp by implementing rmk and rfd following.  
- rmk and rfd implement record types.  
  - { (rmk B*) constructs a record value.  
  - { (rfd E x) projects out the field x of the record value obtained by evaluating E.  
