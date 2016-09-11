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

object Chord{
    sealed trait Message 
    case class setrange(allnode:Array[BigInt]) extends Message
    case class searchid(id:BigInt) extends Message
    
    class Node extends Actor{
        val m=160
        var neighbor=Array.empty[BigInt]     
        var left=BigInt(0)
        var right=BigInt(0)
        def receive={
            case setrange(allnode)=> 
                var i=0 
                while(i<=allnode.length-1){
                    if(BigInt(self.path.name)==allnode(i)&&left!=allnode(i)){
                        left=allnode(i)
                        if(i==allnode.length-1) {right=allnode(0)}
                        else {right=allnode(i+1)}
                    }
                    i=i+1
                }
                var foundleft=right
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
                //}

            case searchid(id)=>
                var next=searchleft(id,neighbor)
                if(next==left) {
                    //println("found"+id)
                    //println(left)
                    //println(right)
                    val counter=context.actorSelection("/user/counter")
                    counter! "found"                    
                }
                else{
                    val counter=context.actorSelection("/user/counter")
                     counter! "hop"
                    //println(self.path.name+"-->"+next)
                    var nextnode=context.actorSelection("/user/"+next.toString)
                    nextnode ! searchid(id)                
                }


                   

            case _ => println(self.path.name+"hey")
        }
        
    }

    class Counter(m:Int,n:Int) extends Actor{
        var counterhop=0
        var foundcounter=0
        val system =ActorSystem("mysystem")
        def receive={
            case "hop"=> counterhop=counterhop+1
            case "found"=> 
                foundcounter=foundcounter+1
                if(foundcounter==m*n) {
                    println((counterhop).toDouble/(m*n).toDouble)
                }
                        
        }         
    }
   
    class Master(nrofnd:Int,nrofrq:Int) extends Actor{
        val system =ActorSystem("mysystem")
        var allnode=Array.empty[BigInt]    
        for(i <- 1 to nrofnd){
            var key=hexToInt(i.toString.sha1.hex)
            //println(i+"  "+key)
            allnode=allnode++Array(key)
            val node=system.actorOf(Props(new Node),name=key.toString)                      
       }
        val counternode=system.actorOf(Props(new Counter(nrofnd,nrofrq)),name="counter")
        for (i <- 1 to nrofnd){
            var key=hexToInt(i.toString.sha1.hex)
            val node=system.actorSelection("/user/"+key)
            node ! setrange(allnode.sorted)
        }

        Thread.sleep(1000);
        for(i<-1 to nrofrq){
            for(j<- 1 to nrofnd){
                val rand=new Random
                var randstr=rand.nextString(10)
                var targetnode=system.actorSelection("/user/"+hexToInt(j.toString.sha1.hex))
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
            val system=ActorSystem("mysystem")
            val Master=system.actorOf(Props(new Master(nrofnd,nrofrq)),name="Master")
            system.terminate()
        }

        catch{
            case _ : Throwable =>
            println("Wrong inputs!!")
            println("Usage: run NumNodes NumRequests")              
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
    
    









