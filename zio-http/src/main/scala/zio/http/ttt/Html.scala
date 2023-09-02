package zio.http.ttt

import zio.http.internal.OutputEncoder
import org.w3c.dom.Attr

sealed trait Html
object Html {

    def raw(raw:String):Node = Node.Text(raw).raw

    implicit def text(text:String):Node = Node.Text(text)

    def empty:Node = Node.Empty

    def node(name:String, children:Html*):Node = Node.Tag(name, children)

    def sequence(sequence: Node*):Node = Node.Sequence(sequence) 

    def attr[A](name:String, a:A)(implicit ev: Attribute.Serializer[A]):Attribute = 
        Attribute(name, ev(a))

    sealed trait Node extends Html { self =>

        import Node._

        private def escape(string:String):String = OutputEncoder.encodeHtml(string)

        def ++ (that: Node):Node = (self, that) match {
            case (Sequence(a), Sequence(b)) => Sequence(a ++ b)
            case (Sequence(a), b) => Sequence(a :+ b)
            case (a, Sequence(b)) => Sequence(a +: b)
            case (a, b) => Sequence(Seq(a, b))
        }

        private def isVoiddd(name: String): Boolean = false

        private[ttt] def encode(state:EncodingState, esc:Boolean = true):String = self match {
            case Raw(node:Node) => node.encode(state, false)
            case Text(text) => if(esc) escape(text) else text
            case Tag(name, children) => 
                val attributes = children.collect {
                    case Html.Attribute(name, value) => s"""$name="${if(esc) escape(value) else value}""""
                }
                val innerState = state.inner
                val elements   = children.collect {
                    case self: Html.Node => self
                }

                val noElements   = elements.isEmpty
                val noAttributes = attributes.isEmpty
                val isVoid       = isVoiddd(name)

                def inner: CharSequence = elements match {
                    case Seq(singleText: Html.Node.Text) => singleText.encode(innerState, esc)
                    case _                         =>
                        s"${innerState.nextElemSeparator}${elements.map(_.encode(innerState, esc)).mkString(innerState.nextElemSeparator)}${state.nextElemSeparator}"
                    }

                if (noElements && noAttributes && isVoid) s"<$name/>"
                else if (noElements && isVoid)
                    s"<$name ${attributes.mkString(" ")}/>"
                else if (noAttributes)
                    s"<$name>$inner</$name>"
                else
                    s"<$name ${attributes.mkString(" ")}>$inner</$name>"

            case Sequence(nodes) =>
                nodes.map(_.encode(state, esc)).mkString(state.nextElemSeparator)
            case Empty => ""
        }

        def encode: String = encode(EncodingState.NoIndentation)

        def encode(spaces:Int):String = encode(EncodingState.Indentation(0, spaces))

        def raw:Node = Raw(self)

    }

    object Node {
        
        private[ttt] case class Raw(node:Node) extends Node
        private[ttt] case class Text(text:String) extends Node
        private[ttt] case class Tag(name:String, children: Seq[Html]) extends Node
        private[ttt] case class Sequence(nodes:Seq[Node]) extends Node
        private[ttt] case object Empty extends Node 

        case class Name(name:String) {
            def apply(children:Html*):Html.Node = Html.Node.Tag(name, children)
        }
    }

    case class Attribute(name:String, value:String) extends Html
    object Attribute {

        sealed trait Serializer[-A] {
            def apply(a:A):String
        }
        
        object Serializer {
            implicit val instanceString = new Serializer[String] {
                override def apply(a: String): String = a
            }
            implicit val instanceList = new Serializer[List[String]] {
                override def apply(a: List[String]): String = a.mkString(" ")
            }
            implicit val instanceTuple = new Serializer[Seq[(String, String)]] {
                override def apply(a: Seq[(String, String)]): String =
                    a.map { case (k, v) => s"""${k}:${v}""" }.mkString(";")
            }
        }

        case class Name[A](name:String)(implicit ev:Serializer[A]) {
            def := (a:A):Html.Attribute = Html.Attribute(name, ev(a))
        }
    }

}

