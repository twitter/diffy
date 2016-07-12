package com.twitter.diffy.proxy.http.filter

import org.jboss.netty.handler.codec.http.HttpHeaders

sealed trait HeaderEffect {
  def apply(headers:HttpHeaders) : Unit
}

case class Rewrite(oldName:String, newName:String) extends HeaderEffect {
  def apply(headers:HttpHeaders): Unit ={
    val temp = headers.get(oldName)
    headers.remove(oldName)
    headers.set(newName, temp)
  }
}

case class Remove( name:String) extends HeaderEffect {
  def apply(headers:HttpHeaders): Unit = {
    headers.remove(name)
  }
}

object HeaderEffect {
  def rewrite(oldName: String, newName:String): HeaderEffect = {
    if (newName.isEmpty){
      Remove(oldName)
    } else {
      Rewrite(oldName,newName)
    }
  }
  def remove(oldName:String):HeaderEffect = Remove(oldName)
}
