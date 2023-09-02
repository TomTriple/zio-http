package zio.http.tpl
import zio.http.tpl.Html.Single
import zio.http.Header
import zio.http.internal.OutputEncoder

sealed trait Html { self => 

    import Html._

    def ++ (that:Html) = (self, that) match {
        case (Empty, b) => b
        case (a, Empty) => a
        case (Single(a), Single(b)) => Multiple(List(a, b))
        case (Single(a), Multiple(b)) => Multiple(a +: b)
        case (Multiple(a), Multiple(b)) => Multiple(a ::: b)
        case (Multiple(a), Single(b)) => Multiple(a :+ b)
    }

    def encode(state:EncodingState):String = 
        self match {
            case Html.Empty                        => ""
            case Html.Single(element)              => element.encode(state)
            case Html.Multiple(elements: Seq[Dom]) => elements.map(_.encode(state)).mkString(state.nextElemSeparator)
        }

    def encode: CharSequence =
        encode(EncodingState.NoIndentation)

    def encode(spaces: Int): CharSequence =
        encode(EncodingState.Indentation(0, spaces))

}

sealed trait Dom { self  =>

    private def escape(html:CharSequence):String = OutputEncoder.encodeHtml(html.toString)
    
    def encode(state: EncodingState, esc: Boolean = true): String = 
      self match {
        case Dom.Element(name, children) =>
          val attributes = children.collect { 
            case Html.Single(self: Dom.Attribute) => self.encode(EncodingState.NoIndentation, esc) 
          }

          val innerState = state.inner
          val elements   = children.collect {
            case self: Dom.Element => self
            case self: Dom.Text    => self
            case self: Dom.Raw    => self
          }

          val noElements   = elements.isEmpty
          val noAttributes = attributes.isEmpty
          val isVoid       = Html.Element.isVoid(name)

          def inner: CharSequence =
            elements match {
              case Seq(singleText: Dom.Text) => singleText.encode(innerState, esc)
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

          case Dom.Text(data)             => if(esc) escape(data) else data
          case Dom.Attribute(name, value) => s"""$name="${if(esc) escape(value) else value}""""
          case Dom.Raw(dom) => dom.encode(state, false)
      }

    def encode: String =
        encode(EncodingState.NoIndentation)

    def encode(spaces: Int): String =
        encode(EncodingState.Indentation(0, spaces))
}

object Dom {
    case class Element(name: String, children:Seq[Html]) extends Dom

    case class Attribute(name: String, value:String) extends Dom

    case class Text(value:String) extends Dom

    case class Raw(dom:Dom) extends Dom
}

object Html {

    implicit def string2html(value:String):Html = 
        Html.text(value)

    object Element {
        case class Name(name:String) {
            def apply(children:Html*) = Html.element(name, children)
        }
        def isVoid(name: CharSequence): Boolean = false        
    }    

    object Attribute {

        sealed trait Serializer[-A] {
            def apply(a:A):String
        }
        
        object Serializer {
            implicit val string = new Serializer[String] {
                override def apply(a: String): String = a
            }
            implicit val list = new Serializer[List[String]] {
                override def apply(a: List[String]): String = a.mkString(";")
            }
            implicit val tuple = new Serializer[Seq[(String, String)]] {
                override def apply(a: Seq[(String, String)]): String =
                    a.map { case (k, v) => s"""${k}:${v}""" }.mkString(";")
            }
        }

        case class Name[A](name:String)(implicit ev: Serializer[A]) {
            def :=(value:A) = Html.attr(name, ev(value))
        }
    }

    def attr[A](name:String, value:String):Html = Single(Dom.Attribute(name, value))

    def element(name:String, children: Seq[Html]):Html = Single(Dom.Element(name, children))

    def text(value:String):Html = Single(Dom.Text(value))

    case class Single(single:Dom) extends Html

    case class Multiple(list:List[Dom]) extends Html

    case object Empty extends Html
}
