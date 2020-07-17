package com.twitter.diffy.lifter

import org.json4s._
import org.json4s.native._
;
import scala.xml.{Elem, Node, NodeSeq}

object XmlLifter {
  def lift(node: NodeSeq): Any = node match {
    case nodeSeq: NodeSeq =>
      JsonLifter.lift(JsonLifter.decode(
        prettyJson(renderJValue(
          decode(nodeSeq.flatMap(node => removeXmlPrefix(node)))
        ))
      ))
  }

  def removeXmlPrefix(node: Node): NodeSeq = {
    node match {
      case n: Elem => {
        val elem: Elem = n.copy(null)
        if (elem.child.length > 0) {
          elem.copy(child = removeXmlPrefix(elem.child(0)))
        } else {
          elem
        }
      }
      case n => n
    }
  }

  def decode(xml: NodeSeq): JValue = Xml.toJson(xml)
}