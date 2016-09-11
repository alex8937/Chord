package project3

import akka.actor._
import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorSystem
import akka.actor.ActorRef
import com.roundeights.hasher.Implicits._
import scala.language.postfixOps
import scala.util.Random
import scala.util.Sorting
import scala.collection.mutable.ArrayBuffer

object Chord{
    sealed trait Message 
    case class setrange(allnode:Array[BigInt]) extends Message
    case class setnewrange(allnode:ArrayBuffer[BigInt]) extends Message
    case class searchid(id:BigInt) extends Message
    
    class Node extends Actor{
        val m=160
        var neighbor=Array.empty[BigInt]     
        var left=BigInt(0)
        var right=BigInt(0)
        var alive=true
        def receive={
            case setrange(allnode)=> 
                var i=0
                var r=1
                while(i<=allnode.length-1){
                    if(BigInt(self.path.name)==allnode(i)&&left!=allnode(i)){
                        left=allnode(i)
                        if(i+r>=allnode.length){
                            right=allnode((i+r)%allnode.length)
                        }
                        //if(i==allnode.length-1) {right=allnode(0)}
                        else {right=allnode(i+r)}
                    }
                    i=i+1
                }
/*                var foundleft=right
                var target=BigInt(0)
                var j=m-1
                while(foundleft!=left){
                    //println(self.path.name+"-->"+j)
                    target=(left+BigInt(2).pow(j))%BigInt(2).pow(m)
                    foundleft=searchleft(target,allnode)
                    //println(self.path.name+"-->"+j+"finish")
                    if(foundleft!=left) {
                        //println(self.path.name+"-->"+j+"finishhere")
                        var k=neighbor.length-1
                        if(k<0) {neighbor=neighbor++Array(foundleft)}
                        else{if(foundleft!=neighbor(k)) neighbor=neighbor++Array(foundleft) }         
                    }
                    //println(self.path.name+"-->"+j+"finish")
                    j=j-1
                }
                neighbor=neighbor++Array(left)
                neighbor=neighbor.sorted
                //for(i<-0 to neighbor.length-1){
                //    println(self.path.name+"-->"+i+"-->"+neighbor(i))
                //}*/
            case setnewrange(allnode)=>
              if(alive==true){
                var allnode1=allnode.toArray
                var foundleft=right
                var target=BigInt(0)
                var j=m-1
                while(foundleft!=left&&j>=0){
                    //println(self.path.name+"-->"+j)
                    target=(left+BigInt(2).pow(j))%BigInt(2).pow(m)
                    foundleft=searchleft(target,allnode1)
                    //println(self.path.name+"-->"+foundleft+j+"finish")
                    if(foundleft!=left) {
                        //println(self.path.name+"-->"+j+"finishhere")
                        var k=neighbor.length-1
                        if(k<0) {neighbor=neighbor++Array(foundleft)}
                        else{if(foundleft!=neighbor(k)) {
                            //println(self.path.name+"-->"+foundleft+j+"finish")
                            neighbor=neighbor++Array(foundleft)                      
                            }
                        }         
                    }
                    //println(self.path.name+"-->"+j+"finish")
                    j=j-1
                }
                neighbor=neighbor++Array(left)
                neighbor=neighbor.sorted
                //println(self.path.name+"-->"+alive)
                //for(i<-0 to neighbor.length-1){ 
                    //println(self.path.name+"-->"+i+"-->"+neighbor(i))
                //}
                //println(self.path.name+"left "+left) 
                //println(right+"right "+left) 
              }                
            case searchid(id)=>
                if(alive==true){
                    var next=searchleft(id,neighbor)
                    //println(self.path.name+"next="+next)
                    if(next==left) {
                        //println("found"+id)
                        //println(left)
                        //println(right)
                        val counter=context.actorSelection("/user/counter")
                        if(belong(id,left,true,right,false)==true){
                            counter! "found"
                            //println("found")
                            }
                        else{counter!"lost"
                             //println("lost")
                            }                    
                    }
                    else{
                        val counter=context.actorSelection("/user/counter")
                        counter! "hop"
                        //println(id+": "+self.path.name+"-->"+next)
                        var nextnode=context.actorSelection("/user/"+next.toString)
                        nextnode ! searchid(id)                
                    }
                }
                else{
                
                    val counter=context.actorSelection("/user/counter")
                    //println("hi")
                    //counter! "lost"
                } 

            case "failed"=>
                alive=false
                //println(self.path.name+"failed")


                   

            case _ => println(self.path.name+"hey")
        }
        
    }

    class Counter(m:Int,n:Int,k:Int) extends Actor{
        var counterhop=0
        var foundcounter=0
        var lost=0
        val system =ActorSystem("mysystem")
        def receive={
            case "hop"=> counterhop=counterhop+1
            case "found"=> 
                foundcounter=foundcounter+1
                if(foundcounter+lost==(m-k)*n) {
                    println((lost).toDouble/((m-k)*n).toDouble)
                }
            case "lost" => 
                lost=lost+1
                if(foundcounter+lost==(m-k)*n) {
                    println((lost).toDouble/((m-k)*n).toDouble)
                }                
                        
        }         
    }
   
    class Master(nrofnd:Int,nrofrq:Int,nroffail:Int) extends Actor{
        val system =ActorSystem("mysystem")
        var allnode=Array.empty[BigInt]
        var allnode1=ArrayBuffer.empty[BigInt]
        for(i <- 1 to nrofnd){
            var key=hexToInt(i.toString.sha1.hex)
            //println(i+"  "+key)
            allnode=allnode++Array(key)
            allnode1=allnode1++Array(key)
            val node=system.actorOf(Props(new Node),name=key.toString)                      
       }
        val counternode=system.actorOf(Props(new Counter(nrofnd,nrofrq,nroffail)),name="counter")
        for (i <- 1 to nrofnd){
            var key=hexToInt(i.toString.sha1.hex)
            val node=system.actorSelection("/user/"+key)
            node ! setrange(allnode.sorted)
        }
        for(i<-1 to nroffail){
            val rand=new Random            
            var randi=rand.nextInt(nrofnd)+1
            allnode1=allnode1--Array(hexToInt(randi.toString.sha1.hex))
            val node=system.actorSelection("/user/"+hexToInt(randi.toString.sha1.hex).toString) 
            node ! "failed"        
        }
        for (i <- 1 to nrofnd){
            var key=hexToInt(i.toString.sha1.hex)
            val node=system.actorSelection("/user/"+key)
            node ! setnewrange(allnode1.sorted)
        }
        Thread.sleep(1000);
        for(i<-1 to nrofrq){
            for(j<- 1 to nrofnd-nroffail){
                val rand=new Random
                var randstr=rand.nextString(10)
                var randi=rand.nextInt(nrofnd-nroffail)
                var targetnode=system.actorSelection("/user/"+allnode1(randi))
                targetnode ! searchid(hexToInt(randstr.sha1.hex))
            }
        }

       def receive={
            case _=> println("MASTER CALL")
        }        
    }



    
    def main(args: Array[String]) {
        try{
            var nrofnd=args(0).toInt  // # of nodes
            var nrofrq=args(1).toInt // # of request
            var nroffail=args(2).toInt // # of failed nodes
            val system=ActorSystem("mysystem")
            val Master=system.actorOf(Props(new Master(nrofnd,nrofrq,nroffail)),name="Master")
            system.terminate()
        }

        catch{
            case _ : Throwable =>
            println("Wrong inputs!!")
            println("Usage: run NumNodes NumRequests Numfailednodes")              
        }
        
    }     
    def hexToInt(s: String): BigInt = {s.toList.map("0123456789abcdef".indexOf(_)).map(BigInt(_)).reduceLeft(_ * 16 + _)}       
    def belong(x:BigInt,a:BigInt,l:Boolean,b:BigInt,r:Boolean):Boolean={
            if(a==b){
                if(l==false&&r==false&&x==a) {return false}
                else {return true}
            }
            else if(a<b){
                if(l==true&&r==true){
                    if(x>=a&&x<=b) return true
                    else {return false}
                }
                else if(l==false&&r==true){
                    if(x>a&&x<=b) return true
                    else {return false}
                }
                else if(l==true&&r==false){
                    if(x>=a&&x<b) return true
                    else {return false}
                }
                else{
                    if(x>a&&x<b) return true
                    else {return false}
                }
            }
            else{
                if(l==true&&r==true){
                    if(x>=a||x<=b) return true
                    else {return false}
                }
                else if(l==false&&r==true){
                    if(x>a||x<=b) return true
                    else {return false}
                }
                else if(l==true&&r==false){
                    if(x>=a||x<b) return true
                    else {return false}
                }
                else{
                    if(x>a||x<b) return true
                    else {return false}
                }
            }     
        }
    def searchleft(goal:BigInt,A:Array[BigInt]):BigInt={
        var A1=A++Array(A(0))
        var m=A.length
        var found=false
        while(found==false){
            
            found=belong(goal,A1(m-1),true,A1(m),false)
                        //println(a+"="+goal+" "+A1(m-1)+" "+A1(m)+" "+found+m)   
            m=m-1  
       
        }
        //println(m)
        return A1(m)
    }
    
}
    
    









