/*
 * Copyright 2021 - 2023 Sporta Technologies PVT LTD & the ZIO HTTP contributors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.http.model

import zio.stacktracer.TracingImplicits.disableAutoTrace // scalafix:ok;

private[zio] trait MimeDB {
  private[zio] lazy val allMediaTypes: List[MediaType] =
    Nil ++ x_shader.all ++ x_conference.all ++ video.all ++ text.all ++ multipart.all ++ model.all ++ message.all ++ image.all ++ font.all ++ chemical.all ++ audio.all ++ application.all
  private val Compressible: Boolean                    = true
  private val Uncompressible: Boolean                  = false
  private val Binary: Boolean                          = true
  private val NotBinary: Boolean                       = false
  private[zio] object application_parts    {
    trait application_0 {
      lazy val `1d-interleaved-parityfec`: MediaType      =
        new MediaType("application", "1d-interleaved-parityfec", Compressible, NotBinary)
      lazy val `3gpdash-qoe-report+xml`: MediaType        =
        new MediaType("application", "3gpdash-qoe-report+xml", Compressible, NotBinary)
      lazy val `3gpp-ims+xml`: MediaType                  =
        new MediaType("application", "3gpp-ims+xml", Compressible, NotBinary)
      lazy val `3gpphal+json`: MediaType                  =
        new MediaType("application", "3gpphal+json", Compressible, NotBinary)
      lazy val `3gpphalforms+json`: MediaType             =
        new MediaType("application", "3gpphalforms+json", Compressible, NotBinary)
      lazy val `a2l`: MediaType                           = new MediaType("application", "a2l", Compressible, NotBinary)
      lazy val `activemessage`: MediaType                 =
        new MediaType("application", "activemessage", Compressible, NotBinary)
      lazy val `activity+json`: MediaType                 =
        new MediaType("application", "activity+json", Compressible, NotBinary)
      lazy val `alto-costmap+json`: MediaType             =
        new MediaType("application", "alto-costmap+json", Compressible, NotBinary)
      lazy val `alto-costmapfilter+json`: MediaType       =
        new MediaType("application", "alto-costmapfilter+json", Compressible, NotBinary)
      lazy val `alto-directory+json`: MediaType           =
        new MediaType("application", "alto-directory+json", Compressible, NotBinary)
      lazy val `alto-endpointcost+json`: MediaType        =
        new MediaType("application", "alto-endpointcost+json", Compressible, NotBinary)
      lazy val `alto-endpointcostparams+json`: MediaType  =
        new MediaType("application", "alto-endpointcostparams+json", Compressible, NotBinary)
      lazy val `alto-endpointprop+json`: MediaType        =
        new MediaType("application", "alto-endpointprop+json", Compressible, NotBinary)
      lazy val `alto-endpointpropparams+json`: MediaType  =
        new MediaType("application", "alto-endpointpropparams+json", Compressible, NotBinary)
      lazy val `alto-error+json`: MediaType               =
        new MediaType("application", "alto-error+json", Compressible, NotBinary)
      lazy val `alto-networkmap+json`: MediaType          =
        new MediaType("application", "alto-networkmap+json", Compressible, NotBinary)
      lazy val `alto-networkmapfilter+json`: MediaType    =
        new MediaType("application", "alto-networkmapfilter+json", Compressible, NotBinary)
      lazy val `alto-updatestreamcontrol+json`: MediaType =
        new MediaType("application", "alto-updatestreamcontrol+json", Compressible, NotBinary)
      lazy val `alto-updatestreamparams+json`: MediaType  =
        new MediaType("application", "alto-updatestreamparams+json", Compressible, NotBinary)
      lazy val `aml`: MediaType                           = new MediaType("application", "aml", Compressible, NotBinary)
      lazy val `andrew-inset`: MediaType                  =
        new MediaType("application", "andrew-inset", Compressible, NotBinary, List("ez"))
      lazy val `applefile`: MediaType                     =
        new MediaType("application", "applefile", Compressible, NotBinary)
      lazy val `applixware`: MediaType                    =
        new MediaType("application", "applixware", Compressible, NotBinary, List("aw"))
      lazy val `atf`: MediaType                           = new MediaType("application", "atf", Compressible, NotBinary)
      lazy val `atfx`: MediaType                       = new MediaType("application", "atfx", Compressible, NotBinary)
      lazy val `atom+xml`: MediaType                   =
        new MediaType("application", "atom+xml", Compressible, NotBinary, List("atom"))
      lazy val `atomcat+xml`: MediaType                =
        new MediaType("application", "atomcat+xml", Compressible, NotBinary, List("atomcat"))
      lazy val `atomdeleted+xml`: MediaType            = new MediaType(
        "application",
        "atomdeleted+xml",
        Compressible,
        NotBinary,
        List("atomdeleted"),
      )
      lazy val `atomicmail`: MediaType                 =
        new MediaType("application", "atomicmail", Compressible, NotBinary)
      lazy val `atomsvc+xml`: MediaType                =
        new MediaType("application", "atomsvc+xml", Compressible, NotBinary, List("atomsvc"))
      lazy val `atsc-dwd+xml`: MediaType               =
        new MediaType("application", "atsc-dwd+xml", Compressible, NotBinary, List("dwd"))
      lazy val `atsc-dynamic-event-message`: MediaType =
        new MediaType("application", "atsc-dynamic-event-message", Compressible, NotBinary)
      lazy val `atsc-held+xml`: MediaType              =
        new MediaType("application", "atsc-held+xml", Compressible, NotBinary, List("held"))
      lazy val `atsc-rdt+json`: MediaType              =
        new MediaType("application", "atsc-rdt+json", Compressible, NotBinary)
      lazy val `atsc-rsat+xml`: MediaType              =
        new MediaType("application", "atsc-rsat+xml", Compressible, NotBinary, List("rsat"))
      lazy val `atxml`: MediaType                      = new MediaType("application", "atxml", Compressible, NotBinary)
      lazy val `auth-policy+xml`: MediaType            =
        new MediaType("application", "auth-policy+xml", Compressible, NotBinary)
      lazy val `bacnet-xdd+zip`: MediaType             =
        new MediaType("application", "bacnet-xdd+zip", Uncompressible, NotBinary)
      lazy val `batch-smtp`: MediaType                 =
        new MediaType("application", "batch-smtp", Compressible, NotBinary)
      lazy val `bdoc`: MediaType                       =
        new MediaType("application", "bdoc", Uncompressible, NotBinary, List("bdoc"))
      lazy val `beep+xml`: MediaType                   =
        new MediaType("application", "beep+xml", Compressible, NotBinary)
      lazy val `calendar+json`: MediaType              =
        new MediaType("application", "calendar+json", Compressible, NotBinary)
      lazy val `calendar+xml`: MediaType               =
        new MediaType("application", "calendar+xml", Compressible, NotBinary, List("xcs"))
      lazy val `call-completion`: MediaType            =
        new MediaType("application", "call-completion", Compressible, NotBinary)
      lazy val `cals-1840`: MediaType                  =
        new MediaType("application", "cals-1840", Compressible, NotBinary)
      lazy val `captive+json`: MediaType               =
        new MediaType("application", "captive+json", Compressible, NotBinary)
      lazy val `cbor`: MediaType                       = new MediaType("application", "cbor", Compressible, NotBinary)
      lazy val `cbor-seq`: MediaType                   =
        new MediaType("application", "cbor-seq", Compressible, NotBinary)
      lazy val `cccex`: MediaType                      = new MediaType("application", "cccex", Compressible, NotBinary)
      lazy val `ccmp+xml`: MediaType                   =
        new MediaType("application", "ccmp+xml", Compressible, NotBinary)
      lazy val `ccxml+xml`: MediaType                  =
        new MediaType("application", "ccxml+xml", Compressible, NotBinary, List("ccxml"))
      lazy val `cdfx+xml`: MediaType                   =
        new MediaType("application", "cdfx+xml", Compressible, NotBinary, List("cdfx"))
      lazy val `cdmi-capability`: MediaType            =
        new MediaType("application", "cdmi-capability", Compressible, NotBinary, List("cdmia"))
      lazy val `cdmi-container`: MediaType             =
        new MediaType("application", "cdmi-container", Compressible, NotBinary, List("cdmic"))
      lazy val `cdmi-domain`: MediaType                =
        new MediaType("application", "cdmi-domain", Compressible, NotBinary, List("cdmid"))
      lazy val `cdmi-object`: MediaType                =
        new MediaType("application", "cdmi-object", Compressible, NotBinary, List("cdmio"))
      lazy val `cdmi-queue`: MediaType                 =
        new MediaType("application", "cdmi-queue", Compressible, NotBinary, List("cdmiq"))
      lazy val `cdni`: MediaType                       = new MediaType("application", "cdni", Compressible, NotBinary)
      lazy val `cea`: MediaType                        = new MediaType("application", "cea", Compressible, NotBinary)
      lazy val `cea-2018+xml`: MediaType               =
        new MediaType("application", "cea-2018+xml", Compressible, NotBinary)
      lazy val `cellml+xml`: MediaType                 =
        new MediaType("application", "cellml+xml", Compressible, NotBinary)
      lazy val `cfw`: MediaType                        = new MediaType("application", "cfw", Compressible, NotBinary)
      lazy val `clr`: MediaType                        = new MediaType("application", "clr", Compressible, NotBinary)
      lazy val `clue+xml`: MediaType                   =
        new MediaType("application", "clue+xml", Compressible, NotBinary)
      lazy val `clue_info+xml`: MediaType              =
        new MediaType("application", "clue_info+xml", Compressible, NotBinary)
      lazy val `cms`: MediaType                        = new MediaType("application", "cms", Compressible, NotBinary)
      lazy val `cnrp+xml`: MediaType                   =
        new MediaType("application", "cnrp+xml", Compressible, NotBinary)
      lazy val `coap-group+json`: MediaType            =
        new MediaType("application", "coap-group+json", Compressible, NotBinary)
      lazy val `coap-payload`: MediaType               =
        new MediaType("application", "coap-payload", Compressible, NotBinary)
      lazy val `commonground`: MediaType               =
        new MediaType("application", "commonground", Compressible, NotBinary)
      lazy val `conference-info+xml`: MediaType        =
        new MediaType("application", "conference-info+xml", Compressible, NotBinary)
      lazy val `cose`: MediaType                       = new MediaType("application", "cose", Compressible, NotBinary)
      lazy val `cose-key`: MediaType                   =
        new MediaType("application", "cose-key", Compressible, NotBinary)
      lazy val `cose-key-set`: MediaType               =
        new MediaType("application", "cose-key-set", Compressible, NotBinary)
      lazy val `cpl+xml`: MediaType                    =
        new MediaType("application", "cpl+xml", Compressible, NotBinary)
      lazy val `csrattrs`: MediaType                   =
        new MediaType("application", "csrattrs", Compressible, NotBinary)
      lazy val `csta+xml`: MediaType                   =
        new MediaType("application", "csta+xml", Compressible, NotBinary)
      lazy val `cstadata+xml`: MediaType               =
        new MediaType("application", "cstadata+xml", Compressible, NotBinary)
      lazy val `csvm+json`: MediaType                  =
        new MediaType("application", "csvm+json", Compressible, NotBinary)
      lazy val `cu-seeme`: MediaType                   =
        new MediaType("application", "cu-seeme", Compressible, NotBinary, List("cu"))
      lazy val `cwt`: MediaType                        = new MediaType("application", "cwt", Compressible, NotBinary)
      lazy val `cybercash`: MediaType                  =
        new MediaType("application", "cybercash", Compressible, NotBinary)
      lazy val `dart`: MediaType                       = new MediaType("application", "dart", Compressible, NotBinary)
      lazy val `dash+xml`: MediaType                   =
        new MediaType("application", "dash+xml", Compressible, NotBinary, List("mpd"))
      lazy val `dashdelta`: MediaType                  =
        new MediaType("application", "dashdelta", Compressible, NotBinary)
      lazy val `davmount+xml`: MediaType               =
        new MediaType("application", "davmount+xml", Compressible, NotBinary, List("davmount"))
      lazy val `dca-rft`: MediaType                    =
        new MediaType("application", "dca-rft", Compressible, NotBinary)
      lazy val `dcd`: MediaType                        = new MediaType("application", "dcd", Compressible, NotBinary)
      lazy val `dec-dx`: MediaType                     = new MediaType("application", "dec-dx", Compressible, NotBinary)
      lazy val `dialog-info+xml`: MediaType            =
        new MediaType("application", "dialog-info+xml", Compressible, NotBinary)
      lazy val `dicom`: MediaType                      = new MediaType("application", "dicom", Compressible, NotBinary)
      lazy val `dicom+json`: MediaType                 =
        new MediaType("application", "dicom+json", Compressible, NotBinary)
      lazy val `dicom+xml`: MediaType                  =
        new MediaType("application", "dicom+xml", Compressible, NotBinary)
      lazy val `dii`: MediaType                        = new MediaType("application", "dii", Compressible, NotBinary)
      lazy val `dit`: MediaType                        = new MediaType("application", "dit", Compressible, NotBinary)
      lazy val `dns`: MediaType                        = new MediaType("application", "dns", Compressible, NotBinary)
      lazy val `dns+json`: MediaType                   =
        new MediaType("application", "dns+json", Compressible, NotBinary)
      lazy val `dns-message`: MediaType                =
        new MediaType("application", "dns-message", Compressible, NotBinary)
      lazy val `docbook+xml`: MediaType                =
        new MediaType("application", "docbook+xml", Compressible, NotBinary, List("dbk"))
      lazy val `dots+cbor`: MediaType                  =
        new MediaType("application", "dots+cbor", Compressible, NotBinary)
      lazy val `dskpp+xml`: MediaType                  =
        new MediaType("application", "dskpp+xml", Compressible, NotBinary)
      lazy val `dssc+der`: MediaType                   =
        new MediaType("application", "dssc+der", Compressible, NotBinary, List("dssc"))
      lazy val `dssc+xml`: MediaType                   =
        new MediaType("application", "dssc+xml", Compressible, NotBinary, List("xdssc"))
      lazy val `dvcs`: MediaType                       = new MediaType("application", "dvcs", Compressible, NotBinary)
      lazy val `ecmascript`: MediaType                 =
        new MediaType("application", "ecmascript", Compressible, NotBinary, List("es", "ecma"))
      lazy val `edi-consent`: MediaType                =
        new MediaType("application", "edi-consent", Compressible, NotBinary)
      lazy val `edi-x12`: MediaType                    =
        new MediaType("application", "edi-x12", Uncompressible, NotBinary)
      lazy val `edifact`: MediaType                    =
        new MediaType("application", "edifact", Uncompressible, NotBinary)
      lazy val `efi`: MediaType                        = new MediaType("application", "efi", Compressible, NotBinary)
      lazy val `elm+json`: MediaType                   =
        new MediaType("application", "elm+json", Compressible, NotBinary)
      lazy val `elm+xml`: MediaType                    =
        new MediaType("application", "elm+xml", Compressible, NotBinary)
      lazy val `emergencycalldata.cap+xml`: MediaType  =
        new MediaType("application", "emergencycalldata.cap+xml", Compressible, NotBinary)
      lazy val `emergencycalldata.comment+xml`: MediaType        =
        new MediaType("application", "emergencycalldata.comment+xml", Compressible, NotBinary)
      lazy val `emergencycalldata.control+xml`: MediaType        =
        new MediaType("application", "emergencycalldata.control+xml", Compressible, NotBinary)
      lazy val `emergencycalldata.deviceinfo+xml`: MediaType     =
        new MediaType("application", "emergencycalldata.deviceinfo+xml", Compressible, NotBinary)
      lazy val `emergencycalldata.ecall.msd`: MediaType          =
        new MediaType("application", "emergencycalldata.ecall.msd", Compressible, NotBinary)
      lazy val `emergencycalldata.providerinfo+xml`: MediaType   =
        new MediaType("application", "emergencycalldata.providerinfo+xml", Compressible, NotBinary)
      lazy val `emergencycalldata.serviceinfo+xml`: MediaType    =
        new MediaType("application", "emergencycalldata.serviceinfo+xml", Compressible, NotBinary)
      lazy val `emergencycalldata.subscriberinfo+xml`: MediaType = new MediaType(
        "application",
        "emergencycalldata.subscriberinfo+xml",
        Compressible,
        NotBinary,
      )
      lazy val `emergencycalldata.veds+xml`: MediaType           =
        new MediaType("application", "emergencycalldata.veds+xml", Compressible, NotBinary)
      lazy val `emma+xml`: MediaType                             =
        new MediaType("application", "emma+xml", Compressible, NotBinary, List("emma"))
      lazy val `emotionml+xml`: MediaType                        =
        new MediaType("application", "emotionml+xml", Compressible, NotBinary, List("emotionml"))
      lazy val `encaprtp`: MediaType                             =
        new MediaType("application", "encaprtp", Compressible, NotBinary)
      lazy val `epp+xml`: MediaType                              =
        new MediaType("application", "epp+xml", Compressible, NotBinary)
      lazy val `epub+zip`: MediaType                             =
        new MediaType("application", "epub+zip", Uncompressible, NotBinary, List("epub"))
      lazy val `eshop`: MediaType                    = new MediaType("application", "eshop", Compressible, NotBinary)
      lazy val `exi`: MediaType                      =
        new MediaType("application", "exi", Compressible, NotBinary, List("exi"))
      lazy val `expect-ct-report+json`: MediaType    =
        new MediaType("application", "expect-ct-report+json", Compressible, NotBinary)
      lazy val `fastinfoset`: MediaType              =
        new MediaType("application", "fastinfoset", Compressible, NotBinary)
      lazy val `fastsoap`: MediaType                 =
        new MediaType("application", "fastsoap", Compressible, NotBinary)
      lazy val `fdt+xml`: MediaType                  =
        new MediaType("application", "fdt+xml", Compressible, NotBinary, List("fdt"))
      lazy val `fhir+json`: MediaType                =
        new MediaType("application", "fhir+json", Compressible, NotBinary)
      lazy val `fhir+xml`: MediaType                 =
        new MediaType("application", "fhir+xml", Compressible, NotBinary)
      lazy val `fido.trusted-apps+json`: MediaType   =
        new MediaType("application", "fido.trusted-apps+json", Compressible, NotBinary)
      lazy val `fits`: MediaType                     = new MediaType("application", "fits", Compressible, NotBinary)
      lazy val `flexfec`: MediaType                  =
        new MediaType("application", "flexfec", Compressible, NotBinary)
      lazy val `font-sfnt`: MediaType                =
        new MediaType("application", "font-sfnt", Compressible, NotBinary)
      lazy val `font-tdpfr`: MediaType               =
        new MediaType("application", "font-tdpfr", Compressible, NotBinary, List("pfr"))
      lazy val `font-woff`: MediaType                =
        new MediaType("application", "font-woff", Uncompressible, Binary)
      lazy val `framework-attributes+xml`: MediaType =
        new MediaType("application", "framework-attributes+xml", Compressible, NotBinary)
      lazy val `geo+json`: MediaType                 =
        new MediaType("application", "geo+json", Compressible, NotBinary, List("geojson"))
      lazy val `geo+json-seq`: MediaType             =
        new MediaType("application", "geo+json-seq", Compressible, NotBinary)
      lazy val `geopackage+sqlite3`: MediaType       =
        new MediaType("application", "geopackage+sqlite3", Compressible, NotBinary)
      lazy val `geoxacml+xml`: MediaType             =
        new MediaType("application", "geoxacml+xml", Compressible, NotBinary)
      lazy val `gltf-buffer`: MediaType              =
        new MediaType("application", "gltf-buffer", Compressible, NotBinary)
      lazy val `gml+xml`: MediaType                  =
        new MediaType("application", "gml+xml", Compressible, NotBinary, List("gml"))
      lazy val `gpx+xml`: MediaType                  =
        new MediaType("application", "gpx+xml", Compressible, NotBinary, List("gpx"))
      lazy val `gxf`: MediaType                      =
        new MediaType("application", "gxf", Compressible, NotBinary, List("gxf"))
      lazy val `gzip`: MediaType                     =
        new MediaType("application", "gzip", Uncompressible, Binary, List("gz"))
      lazy val `h224`: MediaType                     = new MediaType("application", "h224", Compressible, NotBinary)
      lazy val `held+xml`: MediaType                 =
        new MediaType("application", "held+xml", Compressible, NotBinary)
      lazy val `hjson`: MediaType                    =
        new MediaType("application", "hjson", Compressible, NotBinary, List("hjson"))
      lazy val `http`: MediaType                     = new MediaType("application", "http", Compressible, NotBinary)
      lazy val `hyperstudio`: MediaType              =
        new MediaType("application", "hyperstudio", Compressible, NotBinary, List("stk"))
      lazy val `ibe-key-request+xml`: MediaType      =
        new MediaType("application", "ibe-key-request+xml", Compressible, NotBinary)
      lazy val `ibe-pkg-reply+xml`: MediaType        =
        new MediaType("application", "ibe-pkg-reply+xml", Compressible, NotBinary)
      lazy val `ibe-pp-data`: MediaType              =
        new MediaType("application", "ibe-pp-data", Compressible, NotBinary)
      lazy val `iges`: MediaType                     = new MediaType("application", "iges", Compressible, NotBinary)
      lazy val `im-iscomposing+xml`: MediaType       =
        new MediaType("application", "im-iscomposing+xml", Compressible, NotBinary)
      lazy val `index`: MediaType                    = new MediaType("application", "index", Compressible, NotBinary)
      lazy val `index.cmd`: MediaType                =
        new MediaType("application", "index.cmd", Compressible, NotBinary)
      lazy val `index.obj`: MediaType                =
        new MediaType("application", "index.obj", Compressible, NotBinary)
      lazy val `index.response`: MediaType           =
        new MediaType("application", "index.response", Compressible, NotBinary)
      lazy val `index.vnd`: MediaType                =
        new MediaType("application", "index.vnd", Compressible, NotBinary)
      lazy val `inkml+xml`: MediaType                =
        new MediaType("application", "inkml+xml", Compressible, NotBinary, List("ink", "inkml"))
      lazy val `iotp`: MediaType                     = new MediaType("application", "iotp", Compressible, NotBinary)
      lazy val `ipfix`: MediaType                    =
        new MediaType("application", "ipfix", Compressible, NotBinary, List("ipfix"))
      lazy val `ipp`: MediaType                      = new MediaType("application", "ipp", Compressible, NotBinary)
      lazy val `isup`: MediaType                     = new MediaType("application", "isup", Compressible, NotBinary)
      lazy val `its+xml`: MediaType                  =
        new MediaType("application", "its+xml", Compressible, NotBinary, List("its"))
      lazy val `java-archive`: MediaType             = new MediaType(
        "application",
        "java-archive",
        Uncompressible,
        Binary,
        List("jar", "war", "ear"),
      )
      lazy val `java-serialized-object`: MediaType   = new MediaType(
        "application",
        "java-serialized-object",
        Uncompressible,
        NotBinary,
        List("ser"),
      )
      lazy val `java-vm`: MediaType                  =
        new MediaType("application", "java-vm", Uncompressible, NotBinary, List("class"))
      lazy val `javascript`: MediaType               =
        new MediaType("application", "javascript", Compressible, NotBinary, List("js", "mjs"))
      lazy val `jf2feed+json`: MediaType             =
        new MediaType("application", "jf2feed+json", Compressible, NotBinary)
      lazy val `jose`: MediaType                     = new MediaType("application", "jose", Compressible, NotBinary)
      lazy val `jose+json`: MediaType                =
        new MediaType("application", "jose+json", Compressible, NotBinary)
      lazy val `jrd+json`: MediaType                 =
        new MediaType("application", "jrd+json", Compressible, NotBinary)
      lazy val `jscalendar+json`: MediaType          =
        new MediaType("application", "jscalendar+json", Compressible, NotBinary)
      lazy val `json`: MediaType                     =
        new MediaType("application", "json", Compressible, Binary, List("json", "map"))
      lazy val `json-patch+json`: MediaType          =
        new MediaType("application", "json-patch+json", Compressible, NotBinary)
      lazy val `json-seq`: MediaType                 =
        new MediaType("application", "json-seq", Compressible, NotBinary)
      lazy val `json5`: MediaType                    =
        new MediaType("application", "json5", Compressible, NotBinary, List("json5"))
      lazy val `jsonml+json`: MediaType              =
        new MediaType("application", "jsonml+json", Compressible, NotBinary, List("jsonml"))
      lazy val `jwk+json`: MediaType                 =
        new MediaType("application", "jwk+json", Compressible, NotBinary)
      lazy val `jwk-set+json`: MediaType             =
        new MediaType("application", "jwk-set+json", Compressible, NotBinary)
      lazy val `jwt`: MediaType                      = new MediaType("application", "jwt", Compressible, NotBinary)
      lazy val `kpml-request+xml`: MediaType         =
        new MediaType("application", "kpml-request+xml", Compressible, NotBinary)
      lazy val `kpml-response+xml`: MediaType        =
        new MediaType("application", "kpml-response+xml", Compressible, NotBinary)
      lazy val `ld+json`: MediaType                  =
        new MediaType("application", "ld+json", Compressible, NotBinary, List("jsonld"))
      lazy val `lgr+xml`: MediaType                  =
        new MediaType("application", "lgr+xml", Compressible, NotBinary, List("lgr"))
      lazy val `link-format`: MediaType              =
        new MediaType("application", "link-format", Compressible, NotBinary)
      lazy val `load-control+xml`: MediaType         =
        new MediaType("application", "load-control+xml", Compressible, NotBinary)
      lazy val `lost+xml`: MediaType                 =
        new MediaType("application", "lost+xml", Compressible, NotBinary, List("lostxml"))
      lazy val `lostsync+xml`: MediaType             =
        new MediaType("application", "lostsync+xml", Compressible, NotBinary)
      lazy val `lpf+zip`: MediaType                  =
        new MediaType("application", "lpf+zip", Uncompressible, NotBinary)
      lazy val `lxf`: MediaType                      = new MediaType("application", "lxf", Compressible, NotBinary)
      lazy val `mac-binhex40`: MediaType             =
        new MediaType("application", "mac-binhex40", Compressible, NotBinary, List("hqx"))
      lazy val `mac-compactpro`: MediaType           =
        new MediaType("application", "mac-compactpro", Compressible, NotBinary, List("cpt"))
      lazy val `macwriteii`: MediaType               =
        new MediaType("application", "macwriteii", Compressible, NotBinary)
      lazy val `mads+xml`: MediaType                 =
        new MediaType("application", "mads+xml", Compressible, NotBinary, List("mads"))
      lazy val `manifest+json`: MediaType            =
        new MediaType("application", "manifest+json", Compressible, NotBinary, List("webmanifest"))
      lazy val `marc`: MediaType                     =
        new MediaType("application", "marc", Compressible, NotBinary, List("mrc"))
      lazy val `marcxml+xml`: MediaType              =
        new MediaType("application", "marcxml+xml", Compressible, NotBinary, List("mrcx"))
      lazy val `mathematica`: MediaType              =
        new MediaType("application", "mathematica", Compressible, NotBinary, List("ma", "nb", "mb"))
      lazy val `mathml+xml`: MediaType               =
        new MediaType("application", "mathml+xml", Compressible, NotBinary, List("mathml"))
      lazy val `mathml-content+xml`: MediaType       =
        new MediaType("application", "mathml-content+xml", Compressible, NotBinary)
      lazy val `mathml-presentation+xml`: MediaType  =
        new MediaType("application", "mathml-presentation+xml", Compressible, NotBinary)
      lazy val `mbms-associated-procedure-description+xml`: MediaType = new MediaType(
        "application",
        "mbms-associated-procedure-description+xml",
        Compressible,
        NotBinary,
      )
      lazy val `mbms-deregister+xml`: MediaType                       =
        new MediaType("application", "mbms-deregister+xml", Compressible, NotBinary)
      lazy val `mbms-envelope+xml`: MediaType                         =
        new MediaType("application", "mbms-envelope+xml", Compressible, NotBinary)
      lazy val `mbms-msk+xml`: MediaType                              =
        new MediaType("application", "mbms-msk+xml", Compressible, NotBinary)
      lazy val `mbms-msk-response+xml`: MediaType                     =
        new MediaType("application", "mbms-msk-response+xml", Compressible, NotBinary)
      lazy val `mbms-protection-description+xml`: MediaType           =
        new MediaType("application", "mbms-protection-description+xml", Compressible, NotBinary)
      lazy val `mbms-reception-report+xml`: MediaType                 =
        new MediaType("application", "mbms-reception-report+xml", Compressible, NotBinary)
      lazy val `mbms-register+xml`: MediaType                         =
        new MediaType("application", "mbms-register+xml", Compressible, NotBinary)
      lazy val `mbms-register-response+xml`: MediaType                =
        new MediaType("application", "mbms-register-response+xml", Compressible, NotBinary)
      lazy val `mbms-schedule+xml`: MediaType                         =
        new MediaType("application", "mbms-schedule+xml", Compressible, NotBinary)
      lazy val `mbms-user-service-description+xml`: MediaType         =
        new MediaType("application", "mbms-user-service-description+xml", Compressible, NotBinary)
      lazy val `mbox`: MediaType                                      =
        new MediaType("application", "mbox", Compressible, NotBinary, List("mbox"))
      lazy val `media-policy-dataset+xml`: MediaType                  =
        new MediaType("application", "media-policy-dataset+xml", Compressible, NotBinary)
      lazy val `media_control+xml`: MediaType                         =
        new MediaType("application", "media_control+xml", Compressible, NotBinary)
      lazy val `mediaservercontrol+xml`: MediaType                    = new MediaType(
        "application",
        "mediaservercontrol+xml",
        Compressible,
        NotBinary,
        List("mscml"),
      )
      lazy val `merge-patch+json`: MediaType                          =
        new MediaType("application", "merge-patch+json", Compressible, NotBinary)
      lazy val `metalink+xml`: MediaType                              =
        new MediaType("application", "metalink+xml", Compressible, NotBinary, List("metalink"))
      lazy val `metalink4+xml`: MediaType                             =
        new MediaType("application", "metalink4+xml", Compressible, NotBinary, List("meta4"))
      lazy val `mets+xml`: MediaType                                  =
        new MediaType("application", "mets+xml", Compressible, NotBinary, List("mets"))
      lazy val `mf4`: MediaType                     = new MediaType("application", "mf4", Compressible, NotBinary)
      lazy val `mikey`: MediaType                   = new MediaType("application", "mikey", Compressible, NotBinary)
      lazy val `mipc`: MediaType                    = new MediaType("application", "mipc", Compressible, NotBinary)
      lazy val `missing-blocks+cbor-seq`: MediaType =
        new MediaType("application", "missing-blocks+cbor-seq", Compressible, NotBinary)
      lazy val `mmt-aei+xml`: MediaType             =
        new MediaType("application", "mmt-aei+xml", Compressible, NotBinary, List("maei"))
      lazy val `mmt-usd+xml`: MediaType             =
        new MediaType("application", "mmt-usd+xml", Compressible, NotBinary, List("musd"))
      lazy val `mods+xml`: MediaType                =
        new MediaType("application", "mods+xml", Compressible, NotBinary, List("mods"))
      lazy val `moss-keys`: MediaType               =
        new MediaType("application", "moss-keys", Compressible, NotBinary)
      lazy val `moss-signature`: MediaType          =
        new MediaType("application", "moss-signature", Compressible, NotBinary)
      lazy val `mosskey-data`: MediaType            =
        new MediaType("application", "mosskey-data", Compressible, NotBinary)
      lazy val `mosskey-request`: MediaType         =
        new MediaType("application", "mosskey-request", Compressible, NotBinary)
      lazy val `mp21`: MediaType                    =
        new MediaType("application", "mp21", Compressible, NotBinary, List("m21", "mp21"))
      lazy val `mp4`: MediaType                     =
        new MediaType("application", "mp4", Compressible, NotBinary, List("mp4s", "m4p"))
      lazy val `mpeg4-generic`: MediaType           =
        new MediaType("application", "mpeg4-generic", Compressible, NotBinary)
      lazy val `mpeg4-iod`: MediaType               =
        new MediaType("application", "mpeg4-iod", Compressible, NotBinary)
      lazy val `mpeg4-iod-xmt`: MediaType           =
        new MediaType("application", "mpeg4-iod-xmt", Compressible, NotBinary)
      lazy val `mrb-consumer+xml`: MediaType        =
        new MediaType("application", "mrb-consumer+xml", Compressible, NotBinary)
      lazy val `mrb-publish+xml`: MediaType         =
        new MediaType("application", "mrb-publish+xml", Compressible, NotBinary)
      lazy val `msc-ivr+xml`: MediaType             =
        new MediaType("application", "msc-ivr+xml", Compressible, NotBinary)
      lazy val `msc-mixer+xml`: MediaType           =
        new MediaType("application", "msc-mixer+xml", Compressible, NotBinary)
      lazy val `msword`: MediaType                  =
        new MediaType("application", "msword", Uncompressible, Binary, List("doc", "dot"))
      lazy val `mud+json`: MediaType                =
        new MediaType("application", "mud+json", Compressible, NotBinary)
      lazy val `multipart-core`: MediaType          =
        new MediaType("application", "multipart-core", Compressible, NotBinary)
      lazy val `mxf`: MediaType                     =
        new MediaType("application", "mxf", Compressible, NotBinary, List("mxf"))
      lazy val `n-quads`: MediaType                 =
        new MediaType("application", "n-quads", Compressible, NotBinary, List("nq"))
      lazy val `n-triples`: MediaType               =
        new MediaType("application", "n-triples", Compressible, NotBinary, List("nt"))
      lazy val `nasdata`: MediaType                 =
        new MediaType("application", "nasdata", Compressible, NotBinary)
      lazy val `news-checkgroups`: MediaType        =
        new MediaType("application", "news-checkgroups", Compressible, NotBinary)
      lazy val `news-groupinfo`: MediaType          =
        new MediaType("application", "news-groupinfo", Compressible, NotBinary)
      lazy val `news-transmission`: MediaType       =
        new MediaType("application", "news-transmission", Compressible, NotBinary)
      lazy val `nlsml+xml`: MediaType               =
        new MediaType("application", "nlsml+xml", Compressible, NotBinary)
      lazy val `node`: MediaType                    =
        new MediaType("application", "node", Compressible, NotBinary, List("cjs"))
      lazy val `nss`: MediaType                     = new MediaType("application", "nss", Compressible, NotBinary)
      lazy val `oauth-authz-req+jwt`: MediaType     =
        new MediaType("application", "oauth-authz-req+jwt", Compressible, NotBinary)
      lazy val `ocsp-request`: MediaType            =
        new MediaType("application", "ocsp-request", Compressible, NotBinary)
      lazy val `ocsp-response`: MediaType           =
        new MediaType("application", "ocsp-response", Compressible, NotBinary)
      lazy val `octet-stream`: MediaType            = new MediaType(
        "application",
        "octet-stream",
        Uncompressible,
        Binary,
        List(
          "bin",
          "dms",
          "lrf",
          "mar",
          "so",
          "dist",
          "distz",
          "pkg",
          "bpk",
          "dump",
          "elc",
          "deploy",
          "exe",
          "dll",
          "deb",
          "dmg",
          "iso",
          "img",
          "msi",
          "msp",
          "msm",
          "buffer",
        ),
      )
      lazy val `oda`: MediaType                     =
        new MediaType("application", "oda", Compressible, NotBinary, List("oda"))
      lazy val `odm+xml`: MediaType                 =
        new MediaType("application", "odm+xml", Compressible, NotBinary)
      lazy val `odx`: MediaType                     = new MediaType("application", "odx", Compressible, NotBinary)
      lazy val `oebps-package+xml`: MediaType       =
        new MediaType("application", "oebps-package+xml", Compressible, NotBinary, List("opf"))
      lazy val `ogg`: MediaType                     =
        new MediaType("application", "ogg", Uncompressible, NotBinary, List("ogx"))
      lazy val `omdoc+xml`: MediaType               =
        new MediaType("application", "omdoc+xml", Compressible, NotBinary, List("omdoc"))
      lazy val `onenote`: MediaType                 = new MediaType(
        "application",
        "onenote",
        Compressible,
        NotBinary,
        List("onetoc", "onetoc2", "onetmp", "onepkg"),
      )
      lazy val `opc-nodeset+xml`: MediaType         =
        new MediaType("application", "opc-nodeset+xml", Compressible, NotBinary)
      lazy val `oscore`: MediaType                  = new MediaType("application", "oscore", Compressible, NotBinary)
      lazy val `oxps`: MediaType                    =
        new MediaType("application", "oxps", Compressible, NotBinary, List("oxps"))
      lazy val `p2p-overlay+xml`: MediaType         =
        new MediaType("application", "p2p-overlay+xml", Compressible, NotBinary, List("relo"))
      lazy val `parityfec`: MediaType               =
        new MediaType("application", "parityfec", Compressible, NotBinary)
      lazy val `passport`: MediaType                =
        new MediaType("application", "passport", Compressible, NotBinary)
      lazy val `patch-ops-error+xml`: MediaType     =
        new MediaType("application", "patch-ops-error+xml", Compressible, NotBinary, List("xer"))
      lazy val `pdf`: MediaType                     =
        new MediaType("application", "pdf", Uncompressible, Binary, List("pdf"))
      lazy val `pdx`: MediaType                     = new MediaType("application", "pdx", Compressible, NotBinary)
      lazy val `pem-certificate-chain`: MediaType   =
        new MediaType("application", "pem-certificate-chain", Compressible, NotBinary)
      lazy val `pgp-encrypted`: MediaType           =
        new MediaType("application", "pgp-encrypted", Uncompressible, NotBinary, List("pgp"))
      lazy val `pgp-keys`: MediaType                =
        new MediaType("application", "pgp-keys", Compressible, NotBinary)
      lazy val `pgp-signature`: MediaType           =
        new MediaType("application", "pgp-signature", Compressible, NotBinary, List("asc", "sig"))
      lazy val `pics-rules`: MediaType              =
        new MediaType("application", "pics-rules", Compressible, NotBinary, List("prf"))
      lazy val `pidf+xml`: MediaType                =
        new MediaType("application", "pidf+xml", Compressible, NotBinary)
      lazy val `pidf-diff+xml`: MediaType           =
        new MediaType("application", "pidf-diff+xml", Compressible, NotBinary)
      lazy val `pkcs10`: MediaType                  =
        new MediaType("application", "pkcs10", Compressible, NotBinary, List("p10"))
      lazy val `pkcs12`: MediaType                  = new MediaType("application", "pkcs12", Compressible, NotBinary)
      lazy val `pkcs7-mime`: MediaType              =
        new MediaType("application", "pkcs7-mime", Compressible, NotBinary, List("p7m", "p7c"))
      lazy val `pkcs7-signature`: MediaType         =
        new MediaType("application", "pkcs7-signature", Compressible, NotBinary, List("p7s"))
      lazy val `pkcs8`: MediaType                   =
        new MediaType("application", "pkcs8", Compressible, NotBinary, List("p8"))
      lazy val `pkcs8-encrypted`: MediaType         =
        new MediaType("application", "pkcs8-encrypted", Compressible, NotBinary)
      lazy val `pkix-attr-cert`: MediaType          =
        new MediaType("application", "pkix-attr-cert", Compressible, NotBinary, List("ac"))
      lazy val `pkix-cert`: MediaType               =
        new MediaType("application", "pkix-cert", Compressible, NotBinary, List("cer"))
      lazy val `pkix-crl`: MediaType                =
        new MediaType("application", "pkix-crl", Compressible, NotBinary, List("crl"))
      lazy val `pkix-pkipath`: MediaType            =
        new MediaType("application", "pkix-pkipath", Compressible, NotBinary, List("pkipath"))
      lazy val `pkixcmp`: MediaType                 =
        new MediaType("application", "pkixcmp", Compressible, NotBinary, List("pki"))
      lazy val `pls+xml`: MediaType                 =
        new MediaType("application", "pls+xml", Compressible, NotBinary, List("pls"))
      lazy val `poc-settings+xml`: MediaType        =
        new MediaType("application", "poc-settings+xml", Compressible, NotBinary)
      lazy val `postscript`: MediaType              =
        new MediaType("application", "postscript", Compressible, Binary, List("ai", "eps", "ps"))
      lazy val `ppsp-tracker+json`: MediaType       =
        new MediaType("application", "ppsp-tracker+json", Compressible, NotBinary)
      lazy val `problem+json`: MediaType            =
        new MediaType("application", "problem+json", Compressible, Binary)
      lazy val `problem+xml`: MediaType             =
        new MediaType("application", "problem+xml", Compressible, NotBinary)
      lazy val `provenance+xml`: MediaType          =
        new MediaType("application", "provenance+xml", Compressible, NotBinary, List("provx"))
      lazy val `prs.alvestrand.titrax-sheet`: MediaType =
        new MediaType("application", "prs.alvestrand.titrax-sheet", Compressible, NotBinary)
      lazy val `prs.cww`: MediaType                     =
        new MediaType("application", "prs.cww", Compressible, NotBinary, List("cww"))
      lazy val `prs.cyn`: MediaType                     =
        new MediaType("application", "prs.cyn", Compressible, NotBinary)
      lazy val `prs.hpub+zip`: MediaType                =
        new MediaType("application", "prs.hpub+zip", Uncompressible, NotBinary)
      lazy val `prs.nprend`: MediaType                  =
        new MediaType("application", "prs.nprend", Compressible, NotBinary)
      lazy val `prs.plucker`: MediaType                 =
        new MediaType("application", "prs.plucker", Compressible, NotBinary)
      lazy val `prs.rdf-xml-crypt`: MediaType           =
        new MediaType("application", "prs.rdf-xml-crypt", Compressible, NotBinary)
      lazy val `prs.xsf+xml`: MediaType                 =
        new MediaType("application", "prs.xsf+xml", Compressible, NotBinary)
      lazy val `pskc+xml`: MediaType                    =
        new MediaType("application", "pskc+xml", Compressible, NotBinary, List("pskcxml"))
      lazy val `pvd+json`: MediaType                    =
        new MediaType("application", "pvd+json", Compressible, NotBinary)
      lazy val `qsig`: MediaType                        = new MediaType("application", "qsig", Compressible, NotBinary)
      lazy val `raml+yaml`: MediaType                   =
        new MediaType("application", "raml+yaml", Compressible, NotBinary, List("raml"))
      lazy val `raptorfec`: MediaType                   =
        new MediaType("application", "raptorfec", Compressible, NotBinary)
      lazy val `rdap+json`: MediaType                   =
        new MediaType("application", "rdap+json", Compressible, NotBinary)
      lazy val `rdf+xml`: MediaType                     =
        new MediaType("application", "rdf+xml", Compressible, NotBinary, List("rdf", "owl"))
      lazy val `reginfo+xml`: MediaType                 =
        new MediaType("application", "reginfo+xml", Compressible, NotBinary, List("rif"))
      lazy val `relax-ng-compact-syntax`: MediaType     = new MediaType(
        "application",
        "relax-ng-compact-syntax",
        Compressible,
        NotBinary,
        List("rnc"),
      )
      lazy val `remote-printing`: MediaType             =
        new MediaType("application", "remote-printing", Compressible, NotBinary)
      lazy val `reputon+json`: MediaType                =
        new MediaType("application", "reputon+json", Compressible, NotBinary)
      lazy val `resource-lists+xml`: MediaType          =
        new MediaType("application", "resource-lists+xml", Compressible, NotBinary, List("rl"))
      lazy val `resource-lists-diff+xml`: MediaType     = new MediaType(
        "application",
        "resource-lists-diff+xml",
        Compressible,
        NotBinary,
        List("rld"),
      )
      lazy val `rfc+xml`: MediaType                     =
        new MediaType("application", "rfc+xml", Compressible, NotBinary)
      lazy val `riscos`: MediaType            = new MediaType("application", "riscos", Compressible, NotBinary)
      lazy val `rlmi+xml`: MediaType          =
        new MediaType("application", "rlmi+xml", Compressible, NotBinary)
      lazy val `rls-services+xml`: MediaType  =
        new MediaType("application", "rls-services+xml", Compressible, NotBinary, List("rs"))
      lazy val `route-apd+xml`: MediaType     =
        new MediaType("application", "route-apd+xml", Compressible, NotBinary, List("rapd"))
      lazy val `route-s-tsid+xml`: MediaType  =
        new MediaType("application", "route-s-tsid+xml", Compressible, NotBinary, List("sls"))
      lazy val `route-usd+xml`: MediaType     =
        new MediaType("application", "route-usd+xml", Compressible, NotBinary, List("rusd"))
      lazy val `rpki-ghostbusters`: MediaType =
        new MediaType("application", "rpki-ghostbusters", Compressible, NotBinary, List("gbr"))
      lazy val `rpki-manifest`: MediaType     =
        new MediaType("application", "rpki-manifest", Compressible, NotBinary, List("mft"))
      lazy val `rpki-publication`: MediaType  =
        new MediaType("application", "rpki-publication", Compressible, NotBinary)
      lazy val `rpki-roa`: MediaType          =
        new MediaType("application", "rpki-roa", Compressible, NotBinary, List("roa"))
      lazy val `rpki-updown`: MediaType       =
        new MediaType("application", "rpki-updown", Compressible, NotBinary)
      lazy val `rsd+xml`: MediaType           =
        new MediaType("application", "rsd+xml", Compressible, NotBinary, List("rsd"))
      lazy val `rss+xml`: MediaType           =
        new MediaType("application", "rss+xml", Compressible, NotBinary, List("rss"))
      lazy val `rtf`: MediaType               =
        new MediaType("application", "rtf", Compressible, NotBinary, List("rtf"))
      lazy val `rtploopback`: MediaType       =
        new MediaType("application", "rtploopback", Compressible, NotBinary)
      lazy val `rtx`: MediaType               = new MediaType("application", "rtx", Compressible, NotBinary)
      lazy val `samlassertion+xml`: MediaType =
        new MediaType("application", "samlassertion+xml", Compressible, NotBinary)
      lazy val `samlmetadata+xml`: MediaType  =
        new MediaType("application", "samlmetadata+xml", Compressible, NotBinary)
      lazy val `sarif+json`: MediaType        =
        new MediaType("application", "sarif+json", Compressible, NotBinary)
      lazy val `sarif-external-properties+json`: MediaType =
        new MediaType("application", "sarif-external-properties+json", Compressible, NotBinary)
      lazy val `sbe`: MediaType                         = new MediaType("application", "sbe", Compressible, NotBinary)
      lazy val `sbml+xml`: MediaType                    =
        new MediaType("application", "sbml+xml", Compressible, NotBinary, List("sbml"))
      lazy val `scaip+xml`: MediaType                   =
        new MediaType("application", "scaip+xml", Compressible, NotBinary)
      lazy val `scim+json`: MediaType                   =
        new MediaType("application", "scim+json", Compressible, NotBinary)
      lazy val `scvp-cv-request`: MediaType             =
        new MediaType("application", "scvp-cv-request", Compressible, NotBinary, List("scq"))
      lazy val `scvp-cv-response`: MediaType            =
        new MediaType("application", "scvp-cv-response", Compressible, NotBinary, List("scs"))
      lazy val `scvp-vp-request`: MediaType             =
        new MediaType("application", "scvp-vp-request", Compressible, NotBinary, List("spq"))
      lazy val `scvp-vp-response`: MediaType            =
        new MediaType("application", "scvp-vp-response", Compressible, NotBinary, List("spp"))
      lazy val `sdp`: MediaType                         =
        new MediaType("application", "sdp", Compressible, NotBinary, List("sdp"))
      lazy val `secevent+jwt`: MediaType                =
        new MediaType("application", "secevent+jwt", Compressible, NotBinary)
      lazy val `senml+cbor`: MediaType                  =
        new MediaType("application", "senml+cbor", Compressible, NotBinary)
      lazy val `senml+json`: MediaType                  =
        new MediaType("application", "senml+json", Compressible, NotBinary)
      lazy val `senml+xml`: MediaType                   =
        new MediaType("application", "senml+xml", Compressible, NotBinary, List("senmlx"))
      lazy val `senml-etch+cbor`: MediaType             =
        new MediaType("application", "senml-etch+cbor", Compressible, NotBinary)
      lazy val `senml-etch+json`: MediaType             =
        new MediaType("application", "senml-etch+json", Compressible, NotBinary)
      lazy val `senml-exi`: MediaType                   =
        new MediaType("application", "senml-exi", Compressible, NotBinary)
      lazy val `sensml+cbor`: MediaType                 =
        new MediaType("application", "sensml+cbor", Compressible, NotBinary)
      lazy val `sensml+json`: MediaType                 =
        new MediaType("application", "sensml+json", Compressible, NotBinary)
      lazy val `sensml+xml`: MediaType                  =
        new MediaType("application", "sensml+xml", Compressible, NotBinary, List("sensmlx"))
      lazy val `sensml-exi`: MediaType                  =
        new MediaType("application", "sensml-exi", Compressible, NotBinary)
      lazy val `sep+xml`: MediaType                     =
        new MediaType("application", "sep+xml", Compressible, NotBinary)
      lazy val `sep-exi`: MediaType                     =
        new MediaType("application", "sep-exi", Compressible, NotBinary)
      lazy val `session-info`: MediaType                =
        new MediaType("application", "session-info", Compressible, NotBinary)
      lazy val `set-payment`: MediaType                 =
        new MediaType("application", "set-payment", Compressible, NotBinary)
      lazy val `set-payment-initiation`: MediaType      = new MediaType(
        "application",
        "set-payment-initiation",
        Compressible,
        NotBinary,
        List("setpay"),
      )
      lazy val `set-registration`: MediaType            =
        new MediaType("application", "set-registration", Compressible, NotBinary)
      lazy val `set-registration-initiation`: MediaType = new MediaType(
        "application",
        "set-registration-initiation",
        Compressible,
        NotBinary,
        List("setreg"),
      )
      lazy val `sgml`: MediaType                        = new MediaType("application", "sgml", Compressible, NotBinary)
      lazy val `sgml-open-catalog`: MediaType           =
        new MediaType("application", "sgml-open-catalog", Compressible, NotBinary)
      lazy val `shf+xml`: MediaType                     =
        new MediaType("application", "shf+xml", Compressible, NotBinary, List("shf"))
      lazy val `sieve`: MediaType                       =
        new MediaType("application", "sieve", Compressible, NotBinary, List("siv", "sieve"))
      lazy val `simple-filter+xml`: MediaType           =
        new MediaType("application", "simple-filter+xml", Compressible, NotBinary)
      lazy val `simple-message-summary`: MediaType      =
        new MediaType("application", "simple-message-summary", Compressible, NotBinary)
      lazy val `simplesymbolcontainer`: MediaType       =
        new MediaType("application", "simplesymbolcontainer", Compressible, NotBinary)
      lazy val `sipc`: MediaType                        = new MediaType("application", "sipc", Compressible, NotBinary)
      lazy val `slate`: MediaType                       = new MediaType("application", "slate", Compressible, NotBinary)
      lazy val `smil`: MediaType                        = new MediaType("application", "smil", Compressible, NotBinary)
      lazy val `smil+xml`: MediaType                    =
        new MediaType("application", "smil+xml", Compressible, NotBinary, List("smi", "smil"))
      lazy val `smpte336m`: MediaType                   =
        new MediaType("application", "smpte336m", Compressible, NotBinary)
      lazy val `soap+fastinfoset`: MediaType            =
        new MediaType("application", "soap+fastinfoset", Compressible, NotBinary)
      lazy val `soap+xml`: MediaType                    =
        new MediaType("application", "soap+xml", Compressible, NotBinary)
      lazy val `sparql-query`: MediaType                =
        new MediaType("application", "sparql-query", Compressible, NotBinary, List("rq"))
      lazy val `sparql-results+xml`: MediaType          =
        new MediaType("application", "sparql-results+xml", Compressible, NotBinary, List("srx"))
      lazy val `spirits-event+xml`: MediaType           =
        new MediaType("application", "spirits-event+xml", Compressible, NotBinary)
      lazy val `sql`: MediaType                         = new MediaType("application", "sql", Compressible, NotBinary)
      lazy val `srgs`: MediaType                        =
        new MediaType("application", "srgs", Compressible, NotBinary, List("gram"))
      lazy val `srgs+xml`: MediaType                    =
        new MediaType("application", "srgs+xml", Compressible, NotBinary, List("grxml"))
      lazy val `sru+xml`: MediaType                     =
        new MediaType("application", "sru+xml", Compressible, NotBinary, List("sru"))
      lazy val `ssdl+xml`: MediaType                    =
        new MediaType("application", "ssdl+xml", Compressible, NotBinary, List("ssdl"))
      lazy val `ssml+xml`: MediaType                    =
        new MediaType("application", "ssml+xml", Compressible, NotBinary, List("ssml"))
      lazy val `stix+json`: MediaType                   =
        new MediaType("application", "stix+json", Compressible, NotBinary)
      lazy val `swid+xml`: MediaType                    =
        new MediaType("application", "swid+xml", Compressible, NotBinary, List("swidtag"))
      lazy val `tamp-apex-update`: MediaType            =
        new MediaType("application", "tamp-apex-update", Compressible, NotBinary)
      lazy val `tamp-apex-update-confirm`: MediaType    =
        new MediaType("application", "tamp-apex-update-confirm", Compressible, NotBinary)
      lazy val `tamp-community-update`: MediaType       =
        new MediaType("application", "tamp-community-update", Compressible, NotBinary)
      lazy val `tamp-community-update-confirm`: MediaType =
        new MediaType("application", "tamp-community-update-confirm", Compressible, NotBinary)
      lazy val `tamp-error`: MediaType                    =
        new MediaType("application", "tamp-error", Compressible, NotBinary)
      lazy val `tamp-sequence-adjust`: MediaType          =
        new MediaType("application", "tamp-sequence-adjust", Compressible, NotBinary)
      lazy val `tamp-sequence-adjust-confirm`: MediaType  =
        new MediaType("application", "tamp-sequence-adjust-confirm", Compressible, NotBinary)
      lazy val `tamp-status-query`: MediaType             =
        new MediaType("application", "tamp-status-query", Compressible, NotBinary)
      lazy val `tamp-status-response`: MediaType          =
        new MediaType("application", "tamp-status-response", Compressible, NotBinary)
      lazy val `tamp-update`: MediaType                   =
        new MediaType("application", "tamp-update", Compressible, NotBinary)
      lazy val `tamp-update-confirm`: MediaType           =
        new MediaType("application", "tamp-update-confirm", Compressible, NotBinary)
      lazy val `tar`: MediaType                           = new MediaType("application", "tar", Compressible, NotBinary)
      lazy val `taxii+json`: MediaType                    =
        new MediaType("application", "taxii+json", Compressible, NotBinary)
      lazy val `td+json`: MediaType                       =
        new MediaType("application", "td+json", Compressible, NotBinary)
      lazy val `tei+xml`: MediaType                       =
        new MediaType("application", "tei+xml", Compressible, NotBinary, List("tei", "teicorpus"))
      lazy val `tetra_isi`: MediaType                     =
        new MediaType("application", "tetra_isi", Compressible, NotBinary)
      lazy val `thraud+xml`: MediaType                    =
        new MediaType("application", "thraud+xml", Compressible, NotBinary, List("tfi"))
      lazy val `timestamp-query`: MediaType               =
        new MediaType("application", "timestamp-query", Compressible, NotBinary)
      lazy val `timestamp-reply`: MediaType               =
        new MediaType("application", "timestamp-reply", Compressible, NotBinary)
      lazy val `timestamped-data`: MediaType              =
        new MediaType("application", "timestamped-data", Compressible, NotBinary, List("tsd"))
      lazy val `tlsrpt+gzip`: MediaType                   =
        new MediaType("application", "tlsrpt+gzip", Compressible, NotBinary)
      lazy val `tlsrpt+json`: MediaType                   =
        new MediaType("application", "tlsrpt+json", Compressible, NotBinary)
      lazy val `tnauthlist`: MediaType                    =
        new MediaType("application", "tnauthlist", Compressible, NotBinary)
      lazy val `toml`: MediaType                          =
        new MediaType("application", "toml", Compressible, NotBinary, List("toml"))
      lazy val `trickle-ice-sdpfrag`: MediaType           =
        new MediaType("application", "trickle-ice-sdpfrag", Compressible, NotBinary)
      lazy val `trig`: MediaType                          =
        new MediaType("application", "trig", Compressible, NotBinary, List("trig"))
      lazy val `ttml+xml`: MediaType                      =
        new MediaType("application", "ttml+xml", Compressible, NotBinary, List("ttml"))
      lazy val `tve-trigger`: MediaType                   =
        new MediaType("application", "tve-trigger", Compressible, NotBinary)
      lazy val `tzif`: MediaType                 = new MediaType("application", "tzif", Compressible, NotBinary)
      lazy val `tzif-leap`: MediaType            =
        new MediaType("application", "tzif-leap", Compressible, NotBinary)
      lazy val `ubjson`: MediaType               =
        new MediaType("application", "ubjson", Uncompressible, NotBinary, List("ubj"))
      lazy val `ulpfec`: MediaType               = new MediaType("application", "ulpfec", Compressible, NotBinary)
      lazy val `urc-grpsheet+xml`: MediaType     =
        new MediaType("application", "urc-grpsheet+xml", Compressible, NotBinary)
      lazy val `urc-ressheet+xml`: MediaType     =
        new MediaType("application", "urc-ressheet+xml", Compressible, NotBinary, List("rsheet"))
      lazy val `urc-targetdesc+xml`: MediaType   =
        new MediaType("application", "urc-targetdesc+xml", Compressible, NotBinary, List("td"))
      lazy val `urc-uisocketdesc+xml`: MediaType =
        new MediaType("application", "urc-uisocketdesc+xml", Compressible, NotBinary)
      lazy val `vcard+json`: MediaType           =
        new MediaType("application", "vcard+json", Compressible, NotBinary)
      lazy val `vcard+xml`: MediaType            =
        new MediaType("application", "vcard+xml", Compressible, NotBinary)
      lazy val `vemmi`: MediaType                = new MediaType("application", "vemmi", Compressible, NotBinary)
      lazy val `vividence.scriptfile`: MediaType =
        new MediaType("application", "vividence.scriptfile", Compressible, NotBinary)
      lazy val `vnd.1000minds.decision-model+xml`: MediaType          = new MediaType(
        "application",
        "vnd.1000minds.decision-model+xml",
        Compressible,
        NotBinary,
        List("1km"),
      )
      lazy val `vnd.3gpp-prose+xml`: MediaType                        =
        new MediaType("application", "vnd.3gpp-prose+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp-prose-pc3ch+xml`: MediaType                  =
        new MediaType("application", "vnd.3gpp-prose-pc3ch+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp-v2x-local-service-information`: MediaType    = new MediaType(
        "application",
        "vnd.3gpp-v2x-local-service-information",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.3gpp.5gnas`: MediaType                            =
        new MediaType("application", "vnd.3gpp.5gnas", Compressible, NotBinary)
      lazy val `vnd.3gpp.access-transfer-events+xml`: MediaType       =
        new MediaType("application", "vnd.3gpp.access-transfer-events+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.bsf+xml`: MediaType                          =
        new MediaType("application", "vnd.3gpp.bsf+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.gmop+xml`: MediaType                         =
        new MediaType("application", "vnd.3gpp.gmop+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.gtpc`: MediaType                             =
        new MediaType("application", "vnd.3gpp.gtpc", Compressible, NotBinary)
      lazy val `vnd.3gpp.interworking-data`: MediaType                =
        new MediaType("application", "vnd.3gpp.interworking-data", Compressible, NotBinary)
      lazy val `vnd.3gpp.lpp`: MediaType                              =
        new MediaType("application", "vnd.3gpp.lpp", Compressible, NotBinary)
      lazy val `vnd.3gpp.mc-signalling-ear`: MediaType                =
        new MediaType("application", "vnd.3gpp.mc-signalling-ear", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcdata-affiliation-command+xml`: MediaType   = new MediaType(
        "application",
        "vnd.3gpp.mcdata-affiliation-command+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.3gpp.mcdata-info+xml`: MediaType                  =
        new MediaType("application", "vnd.3gpp.mcdata-info+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcdata-payload`: MediaType                   =
        new MediaType("application", "vnd.3gpp.mcdata-payload", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcdata-service-config+xml`: MediaType        =
        new MediaType("application", "vnd.3gpp.mcdata-service-config+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcdata-signalling`: MediaType                =
        new MediaType("application", "vnd.3gpp.mcdata-signalling", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcdata-ue-config+xml`: MediaType             =
        new MediaType("application", "vnd.3gpp.mcdata-ue-config+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcdata-user-profile+xml`: MediaType          =
        new MediaType("application", "vnd.3gpp.mcdata-user-profile+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcptt-affiliation-command+xml`: MediaType    = new MediaType(
        "application",
        "vnd.3gpp.mcptt-affiliation-command+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.3gpp.mcptt-floor-request+xml`: MediaType          =
        new MediaType("application", "vnd.3gpp.mcptt-floor-request+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcptt-info+xml`: MediaType                   =
        new MediaType("application", "vnd.3gpp.mcptt-info+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcptt-location-info+xml`: MediaType          =
        new MediaType("application", "vnd.3gpp.mcptt-location-info+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcptt-mbms-usage-info+xml`: MediaType        =
        new MediaType("application", "vnd.3gpp.mcptt-mbms-usage-info+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcptt-service-config+xml`: MediaType         =
        new MediaType("application", "vnd.3gpp.mcptt-service-config+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcptt-signed+xml`: MediaType                 =
        new MediaType("application", "vnd.3gpp.mcptt-signed+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcptt-ue-config+xml`: MediaType              =
        new MediaType("application", "vnd.3gpp.mcptt-ue-config+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcptt-ue-init-config+xml`: MediaType         =
        new MediaType("application", "vnd.3gpp.mcptt-ue-init-config+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcptt-user-profile+xml`: MediaType           =
        new MediaType("application", "vnd.3gpp.mcptt-user-profile+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcvideo-affiliation-command+xml`: MediaType  = new MediaType(
        "application",
        "vnd.3gpp.mcvideo-affiliation-command+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.3gpp.mcvideo-affiliation-info+xml`: MediaType     = new MediaType(
        "application",
        "vnd.3gpp.mcvideo-affiliation-info+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.3gpp.mcvideo-info+xml`: MediaType                 =
        new MediaType("application", "vnd.3gpp.mcvideo-info+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcvideo-location-info+xml`: MediaType        =
        new MediaType("application", "vnd.3gpp.mcvideo-location-info+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcvideo-mbms-usage-info+xml`: MediaType      = new MediaType(
        "application",
        "vnd.3gpp.mcvideo-mbms-usage-info+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.3gpp.mcvideo-service-config+xml`: MediaType       =
        new MediaType("application", "vnd.3gpp.mcvideo-service-config+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcvideo-transmission-request+xml`: MediaType = new MediaType(
        "application",
        "vnd.3gpp.mcvideo-transmission-request+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.3gpp.mcvideo-ue-config+xml`: MediaType            =
        new MediaType("application", "vnd.3gpp.mcvideo-ue-config+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mcvideo-user-profile+xml`: MediaType         =
        new MediaType("application", "vnd.3gpp.mcvideo-user-profile+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.mid-call+xml`: MediaType                     =
        new MediaType("application", "vnd.3gpp.mid-call+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.ngap`: MediaType                             =
        new MediaType("application", "vnd.3gpp.ngap", Compressible, NotBinary)
      lazy val `vnd.3gpp.pfcp`: MediaType                             =
        new MediaType("application", "vnd.3gpp.pfcp", Compressible, NotBinary)
      lazy val `vnd.3gpp.pic-bw-large`: MediaType                     =
        new MediaType("application", "vnd.3gpp.pic-bw-large", Compressible, NotBinary, List("plb"))
      lazy val `vnd.3gpp.pic-bw-small`: MediaType                     =
        new MediaType("application", "vnd.3gpp.pic-bw-small", Compressible, NotBinary, List("psb"))
      lazy val `vnd.3gpp.pic-bw-var`: MediaType                       =
        new MediaType("application", "vnd.3gpp.pic-bw-var", Compressible, NotBinary, List("pvb"))
      lazy val `vnd.3gpp.s1ap`: MediaType                             =
        new MediaType("application", "vnd.3gpp.s1ap", Compressible, NotBinary)
      lazy val `vnd.3gpp.sms`: MediaType                              =
        new MediaType("application", "vnd.3gpp.sms", Compressible, NotBinary)
      lazy val `vnd.3gpp.sms+xml`: MediaType                          =
        new MediaType("application", "vnd.3gpp.sms+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.srvcc-ext+xml`: MediaType                    =
        new MediaType("application", "vnd.3gpp.srvcc-ext+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.srvcc-info+xml`: MediaType                   =
        new MediaType("application", "vnd.3gpp.srvcc-info+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.state-and-event-info+xml`: MediaType         =
        new MediaType("application", "vnd.3gpp.state-and-event-info+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp.ussd+xml`: MediaType                         =
        new MediaType("application", "vnd.3gpp.ussd+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp2.bcmcsinfo+xml`: MediaType                   =
        new MediaType("application", "vnd.3gpp2.bcmcsinfo+xml", Compressible, NotBinary)
      lazy val `vnd.3gpp2.sms`: MediaType                             =
        new MediaType("application", "vnd.3gpp2.sms", Compressible, NotBinary)
      lazy val `vnd.3gpp2.tcap`: MediaType                            =
        new MediaType("application", "vnd.3gpp2.tcap", Compressible, NotBinary, List("tcap"))
      lazy val `vnd.3lightssoftware.imagescal`: MediaType             =
        new MediaType("application", "vnd.3lightssoftware.imagescal", Compressible, NotBinary)
      lazy val `vnd.3m.post-it-notes`: MediaType                      =
        new MediaType("application", "vnd.3m.post-it-notes", Compressible, NotBinary, List("pwn"))
      lazy val `vnd.accpac.simply.aso`: MediaType                     =
        new MediaType("application", "vnd.accpac.simply.aso", Compressible, NotBinary, List("aso"))
      lazy val `vnd.accpac.simply.imp`: MediaType                     =
        new MediaType("application", "vnd.accpac.simply.imp", Compressible, NotBinary, List("imp"))
      lazy val `vnd.acucobol`: MediaType                              =
        new MediaType("application", "vnd.acucobol", Compressible, NotBinary, List("acu"))
      lazy val `vnd.acucorp`: MediaType                               =
        new MediaType("application", "vnd.acucorp", Compressible, NotBinary, List("atc", "acutc"))
      lazy val part_0: List[MediaType]                                = List(
        `1d-interleaved-parityfec`,
        `3gpdash-qoe-report+xml`,
        `3gpp-ims+xml`,
        `3gpphal+json`,
        `3gpphalforms+json`,
        `a2l`,
        `activemessage`,
        `activity+json`,
        `alto-costmap+json`,
        `alto-costmapfilter+json`,
        `alto-directory+json`,
        `alto-endpointcost+json`,
        `alto-endpointcostparams+json`,
        `alto-endpointprop+json`,
        `alto-endpointpropparams+json`,
        `alto-error+json`,
        `alto-networkmap+json`,
        `alto-networkmapfilter+json`,
        `alto-updatestreamcontrol+json`,
        `alto-updatestreamparams+json`,
        `aml`,
        `andrew-inset`,
        `applefile`,
        `applixware`,
        `atf`,
        `atfx`,
        `atom+xml`,
        `atomcat+xml`,
        `atomdeleted+xml`,
        `atomicmail`,
        `atomsvc+xml`,
        `atsc-dwd+xml`,
        `atsc-dynamic-event-message`,
        `atsc-held+xml`,
        `atsc-rdt+json`,
        `atsc-rsat+xml`,
        `atxml`,
        `auth-policy+xml`,
        `bacnet-xdd+zip`,
        `batch-smtp`,
        `bdoc`,
        `beep+xml`,
        `calendar+json`,
        `calendar+xml`,
        `call-completion`,
        `cals-1840`,
        `captive+json`,
        `cbor`,
        `cbor-seq`,
        `cccex`,
        `ccmp+xml`,
        `ccxml+xml`,
        `cdfx+xml`,
        `cdmi-capability`,
        `cdmi-container`,
        `cdmi-domain`,
        `cdmi-object`,
        `cdmi-queue`,
        `cdni`,
        `cea`,
        `cea-2018+xml`,
        `cellml+xml`,
        `cfw`,
        `clr`,
        `clue+xml`,
        `clue_info+xml`,
        `cms`,
        `cnrp+xml`,
        `coap-group+json`,
        `coap-payload`,
        `commonground`,
        `conference-info+xml`,
        `cose`,
        `cose-key`,
        `cose-key-set`,
        `cpl+xml`,
        `csrattrs`,
        `csta+xml`,
        `cstadata+xml`,
        `csvm+json`,
        `cu-seeme`,
        `cwt`,
        `cybercash`,
        `dart`,
        `dash+xml`,
        `dashdelta`,
        `davmount+xml`,
        `dca-rft`,
        `dcd`,
        `dec-dx`,
        `dialog-info+xml`,
        `dicom`,
        `dicom+json`,
        `dicom+xml`,
        `dii`,
        `dit`,
        `dns`,
        `dns+json`,
        `dns-message`,
        `docbook+xml`,
        `dots+cbor`,
        `dskpp+xml`,
        `dssc+der`,
        `dssc+xml`,
        `dvcs`,
        `ecmascript`,
        `edi-consent`,
        `edi-x12`,
        `edifact`,
        `efi`,
        `elm+json`,
        `elm+xml`,
        `emergencycalldata.cap+xml`,
        `emergencycalldata.comment+xml`,
        `emergencycalldata.control+xml`,
        `emergencycalldata.deviceinfo+xml`,
        `emergencycalldata.ecall.msd`,
        `emergencycalldata.providerinfo+xml`,
        `emergencycalldata.serviceinfo+xml`,
        `emergencycalldata.subscriberinfo+xml`,
        `emergencycalldata.veds+xml`,
        `emma+xml`,
        `emotionml+xml`,
        `encaprtp`,
        `epp+xml`,
        `epub+zip`,
        `eshop`,
        `exi`,
        `expect-ct-report+json`,
        `fastinfoset`,
        `fastsoap`,
        `fdt+xml`,
        `fhir+json`,
        `fhir+xml`,
        `fido.trusted-apps+json`,
        `fits`,
        `flexfec`,
        `font-sfnt`,
        `font-tdpfr`,
        `font-woff`,
        `framework-attributes+xml`,
        `geo+json`,
        `geo+json-seq`,
        `geopackage+sqlite3`,
        `geoxacml+xml`,
        `gltf-buffer`,
        `gml+xml`,
        `gpx+xml`,
        `gxf`,
        `gzip`,
        `h224`,
        `held+xml`,
        `hjson`,
        `http`,
        `hyperstudio`,
        `ibe-key-request+xml`,
        `ibe-pkg-reply+xml`,
        `ibe-pp-data`,
        `iges`,
        `im-iscomposing+xml`,
        `index`,
        `index.cmd`,
        `index.obj`,
        `index.response`,
        `index.vnd`,
        `inkml+xml`,
        `iotp`,
        `ipfix`,
        `ipp`,
        `isup`,
        `its+xml`,
        `java-archive`,
        `java-serialized-object`,
        `java-vm`,
        `javascript`,
        `jf2feed+json`,
        `jose`,
        `jose+json`,
        `jrd+json`,
        `jscalendar+json`,
        `json`,
        `json-patch+json`,
        `json-seq`,
        `json5`,
        `jsonml+json`,
        `jwk+json`,
        `jwk-set+json`,
        `jwt`,
        `kpml-request+xml`,
        `kpml-response+xml`,
        `ld+json`,
        `lgr+xml`,
        `link-format`,
        `load-control+xml`,
        `lost+xml`,
        `lostsync+xml`,
        `lpf+zip`,
        `lxf`,
        `mac-binhex40`,
        `mac-compactpro`,
        `macwriteii`,
        `mads+xml`,
        `manifest+json`,
        `marc`,
        `marcxml+xml`,
        `mathematica`,
        `mathml+xml`,
        `mathml-content+xml`,
        `mathml-presentation+xml`,
        `mbms-associated-procedure-description+xml`,
        `mbms-deregister+xml`,
        `mbms-envelope+xml`,
        `mbms-msk+xml`,
        `mbms-msk-response+xml`,
        `mbms-protection-description+xml`,
        `mbms-reception-report+xml`,
        `mbms-register+xml`,
        `mbms-register-response+xml`,
        `mbms-schedule+xml`,
        `mbms-user-service-description+xml`,
        `mbox`,
        `media-policy-dataset+xml`,
        `media_control+xml`,
        `mediaservercontrol+xml`,
        `merge-patch+json`,
        `metalink+xml`,
        `metalink4+xml`,
        `mets+xml`,
        `mf4`,
        `mikey`,
        `mipc`,
        `missing-blocks+cbor-seq`,
        `mmt-aei+xml`,
        `mmt-usd+xml`,
        `mods+xml`,
        `moss-keys`,
        `moss-signature`,
        `mosskey-data`,
        `mosskey-request`,
        `mp21`,
        `mp4`,
        `mpeg4-generic`,
        `mpeg4-iod`,
        `mpeg4-iod-xmt`,
        `mrb-consumer+xml`,
        `mrb-publish+xml`,
        `msc-ivr+xml`,
        `msc-mixer+xml`,
        `msword`,
        `mud+json`,
        `multipart-core`,
        `mxf`,
        `n-quads`,
        `n-triples`,
        `nasdata`,
        `news-checkgroups`,
        `news-groupinfo`,
        `news-transmission`,
        `nlsml+xml`,
        `node`,
        `nss`,
        `oauth-authz-req+jwt`,
        `ocsp-request`,
        `ocsp-response`,
        `octet-stream`,
        `oda`,
        `odm+xml`,
        `odx`,
        `oebps-package+xml`,
        `ogg`,
        `omdoc+xml`,
        `onenote`,
        `opc-nodeset+xml`,
        `oscore`,
        `oxps`,
        `p2p-overlay+xml`,
        `parityfec`,
        `passport`,
        `patch-ops-error+xml`,
        `pdf`,
        `pdx`,
        `pem-certificate-chain`,
        `pgp-encrypted`,
        `pgp-keys`,
        `pgp-signature`,
        `pics-rules`,
        `pidf+xml`,
        `pidf-diff+xml`,
        `pkcs10`,
        `pkcs12`,
        `pkcs7-mime`,
        `pkcs7-signature`,
        `pkcs8`,
        `pkcs8-encrypted`,
        `pkix-attr-cert`,
        `pkix-cert`,
        `pkix-crl`,
        `pkix-pkipath`,
        `pkixcmp`,
        `pls+xml`,
        `poc-settings+xml`,
        `postscript`,
        `ppsp-tracker+json`,
        `problem+json`,
        `problem+xml`,
        `provenance+xml`,
        `prs.alvestrand.titrax-sheet`,
        `prs.cww`,
        `prs.cyn`,
        `prs.hpub+zip`,
        `prs.nprend`,
        `prs.plucker`,
        `prs.rdf-xml-crypt`,
        `prs.xsf+xml`,
        `pskc+xml`,
        `pvd+json`,
        `qsig`,
        `raml+yaml`,
        `raptorfec`,
        `rdap+json`,
        `rdf+xml`,
        `reginfo+xml`,
        `relax-ng-compact-syntax`,
        `remote-printing`,
        `reputon+json`,
        `resource-lists+xml`,
        `resource-lists-diff+xml`,
        `rfc+xml`,
        `riscos`,
        `rlmi+xml`,
        `rls-services+xml`,
        `route-apd+xml`,
        `route-s-tsid+xml`,
        `route-usd+xml`,
        `rpki-ghostbusters`,
        `rpki-manifest`,
        `rpki-publication`,
        `rpki-roa`,
        `rpki-updown`,
        `rsd+xml`,
        `rss+xml`,
        `rtf`,
        `rtploopback`,
        `rtx`,
        `samlassertion+xml`,
        `samlmetadata+xml`,
        `sarif+json`,
        `sarif-external-properties+json`,
        `sbe`,
        `sbml+xml`,
        `scaip+xml`,
        `scim+json`,
        `scvp-cv-request`,
        `scvp-cv-response`,
        `scvp-vp-request`,
        `scvp-vp-response`,
        `sdp`,
        `secevent+jwt`,
        `senml+cbor`,
        `senml+json`,
        `senml+xml`,
        `senml-etch+cbor`,
        `senml-etch+json`,
        `senml-exi`,
        `sensml+cbor`,
        `sensml+json`,
        `sensml+xml`,
        `sensml-exi`,
        `sep+xml`,
        `sep-exi`,
        `session-info`,
        `set-payment`,
        `set-payment-initiation`,
        `set-registration`,
        `set-registration-initiation`,
        `sgml`,
        `sgml-open-catalog`,
        `shf+xml`,
        `sieve`,
        `simple-filter+xml`,
        `simple-message-summary`,
        `simplesymbolcontainer`,
        `sipc`,
        `slate`,
        `smil`,
        `smil+xml`,
        `smpte336m`,
        `soap+fastinfoset`,
        `soap+xml`,
        `sparql-query`,
        `sparql-results+xml`,
        `spirits-event+xml`,
        `sql`,
        `srgs`,
        `srgs+xml`,
        `sru+xml`,
        `ssdl+xml`,
        `ssml+xml`,
        `stix+json`,
        `swid+xml`,
        `tamp-apex-update`,
        `tamp-apex-update-confirm`,
        `tamp-community-update`,
        `tamp-community-update-confirm`,
        `tamp-error`,
        `tamp-sequence-adjust`,
        `tamp-sequence-adjust-confirm`,
        `tamp-status-query`,
        `tamp-status-response`,
        `tamp-update`,
        `tamp-update-confirm`,
        `tar`,
        `taxii+json`,
        `td+json`,
        `tei+xml`,
        `tetra_isi`,
        `thraud+xml`,
        `timestamp-query`,
        `timestamp-reply`,
        `timestamped-data`,
        `tlsrpt+gzip`,
        `tlsrpt+json`,
        `tnauthlist`,
        `toml`,
        `trickle-ice-sdpfrag`,
        `trig`,
        `ttml+xml`,
        `tve-trigger`,
        `tzif`,
        `tzif-leap`,
        `ubjson`,
        `ulpfec`,
        `urc-grpsheet+xml`,
        `urc-ressheet+xml`,
        `urc-targetdesc+xml`,
        `urc-uisocketdesc+xml`,
        `vcard+json`,
        `vcard+xml`,
        `vemmi`,
        `vividence.scriptfile`,
        `vnd.1000minds.decision-model+xml`,
        `vnd.3gpp-prose+xml`,
        `vnd.3gpp-prose-pc3ch+xml`,
        `vnd.3gpp-v2x-local-service-information`,
        `vnd.3gpp.5gnas`,
        `vnd.3gpp.access-transfer-events+xml`,
        `vnd.3gpp.bsf+xml`,
        `vnd.3gpp.gmop+xml`,
        `vnd.3gpp.gtpc`,
        `vnd.3gpp.interworking-data`,
        `vnd.3gpp.lpp`,
        `vnd.3gpp.mc-signalling-ear`,
        `vnd.3gpp.mcdata-affiliation-command+xml`,
        `vnd.3gpp.mcdata-info+xml`,
        `vnd.3gpp.mcdata-payload`,
        `vnd.3gpp.mcdata-service-config+xml`,
        `vnd.3gpp.mcdata-signalling`,
        `vnd.3gpp.mcdata-ue-config+xml`,
        `vnd.3gpp.mcdata-user-profile+xml`,
        `vnd.3gpp.mcptt-affiliation-command+xml`,
        `vnd.3gpp.mcptt-floor-request+xml`,
        `vnd.3gpp.mcptt-info+xml`,
        `vnd.3gpp.mcptt-location-info+xml`,
        `vnd.3gpp.mcptt-mbms-usage-info+xml`,
        `vnd.3gpp.mcptt-service-config+xml`,
        `vnd.3gpp.mcptt-signed+xml`,
        `vnd.3gpp.mcptt-ue-config+xml`,
        `vnd.3gpp.mcptt-ue-init-config+xml`,
        `vnd.3gpp.mcptt-user-profile+xml`,
        `vnd.3gpp.mcvideo-affiliation-command+xml`,
        `vnd.3gpp.mcvideo-affiliation-info+xml`,
        `vnd.3gpp.mcvideo-info+xml`,
        `vnd.3gpp.mcvideo-location-info+xml`,
        `vnd.3gpp.mcvideo-mbms-usage-info+xml`,
        `vnd.3gpp.mcvideo-service-config+xml`,
        `vnd.3gpp.mcvideo-transmission-request+xml`,
        `vnd.3gpp.mcvideo-ue-config+xml`,
        `vnd.3gpp.mcvideo-user-profile+xml`,
        `vnd.3gpp.mid-call+xml`,
        `vnd.3gpp.ngap`,
        `vnd.3gpp.pfcp`,
        `vnd.3gpp.pic-bw-large`,
        `vnd.3gpp.pic-bw-small`,
        `vnd.3gpp.pic-bw-var`,
        `vnd.3gpp.s1ap`,
        `vnd.3gpp.sms`,
        `vnd.3gpp.sms+xml`,
        `vnd.3gpp.srvcc-ext+xml`,
        `vnd.3gpp.srvcc-info+xml`,
        `vnd.3gpp.state-and-event-info+xml`,
        `vnd.3gpp.ussd+xml`,
        `vnd.3gpp2.bcmcsinfo+xml`,
        `vnd.3gpp2.sms`,
        `vnd.3gpp2.tcap`,
        `vnd.3lightssoftware.imagescal`,
        `vnd.3m.post-it-notes`,
        `vnd.accpac.simply.aso`,
        `vnd.accpac.simply.imp`,
        `vnd.acucobol`,
        `vnd.acucorp`,
      )
    }
    trait application_1 {
      lazy val `vnd.adobe.air-application-installer-package+zip`: MediaType = new MediaType(
        "application",
        "vnd.adobe.air-application-installer-package+zip",
        Uncompressible,
        NotBinary,
        List("air"),
      )
      lazy val `vnd.adobe.flash.movie`: MediaType                           =
        new MediaType("application", "vnd.adobe.flash.movie", Compressible, NotBinary)
      lazy val `vnd.adobe.formscentral.fcdt`: MediaType                     = new MediaType(
        "application",
        "vnd.adobe.formscentral.fcdt",
        Compressible,
        NotBinary,
        List("fcdt"),
      )
      lazy val `vnd.adobe.fxp`: MediaType                                   =
        new MediaType("application", "vnd.adobe.fxp", Compressible, NotBinary, List("fxp", "fxpl"))
      lazy val `vnd.adobe.partial-upload`: MediaType                        =
        new MediaType("application", "vnd.adobe.partial-upload", Compressible, NotBinary)
      lazy val `vnd.adobe.xdp+xml`: MediaType                               =
        new MediaType("application", "vnd.adobe.xdp+xml", Compressible, NotBinary, List("xdp"))
      lazy val `vnd.adobe.xfdf`: MediaType                                  =
        new MediaType("application", "vnd.adobe.xfdf", Compressible, NotBinary, List("xfdf"))
      lazy val `vnd.aether.imp`: MediaType                                  =
        new MediaType("application", "vnd.aether.imp", Compressible, NotBinary)
      lazy val `vnd.afpc.afplinedata`: MediaType                            =
        new MediaType("application", "vnd.afpc.afplinedata", Compressible, NotBinary)
      lazy val `vnd.afpc.afplinedata-pagedef`: MediaType                    =
        new MediaType("application", "vnd.afpc.afplinedata-pagedef", Compressible, NotBinary)
      lazy val `vnd.afpc.cmoca-cmresource`: MediaType                       =
        new MediaType("application", "vnd.afpc.cmoca-cmresource", Compressible, NotBinary)
      lazy val `vnd.afpc.foca-charset`: MediaType                           =
        new MediaType("application", "vnd.afpc.foca-charset", Compressible, NotBinary)
      lazy val `vnd.afpc.foca-codedfont`: MediaType                         =
        new MediaType("application", "vnd.afpc.foca-codedfont", Compressible, NotBinary)
      lazy val `vnd.afpc.foca-codepage`: MediaType                          =
        new MediaType("application", "vnd.afpc.foca-codepage", Compressible, NotBinary)
      lazy val `vnd.afpc.modca`: MediaType                                  =
        new MediaType("application", "vnd.afpc.modca", Compressible, NotBinary)
      lazy val `vnd.afpc.modca-cmtable`: MediaType                          =
        new MediaType("application", "vnd.afpc.modca-cmtable", Compressible, NotBinary)
      lazy val `vnd.afpc.modca-formdef`: MediaType                          =
        new MediaType("application", "vnd.afpc.modca-formdef", Compressible, NotBinary)
      lazy val `vnd.afpc.modca-mediummap`: MediaType                        =
        new MediaType("application", "vnd.afpc.modca-mediummap", Compressible, NotBinary)
      lazy val `vnd.afpc.modca-objectcontainer`: MediaType                  =
        new MediaType("application", "vnd.afpc.modca-objectcontainer", Compressible, NotBinary)
      lazy val `vnd.afpc.modca-overlay`: MediaType                          =
        new MediaType("application", "vnd.afpc.modca-overlay", Compressible, NotBinary)
      lazy val `vnd.afpc.modca-pagesegment`: MediaType                      =
        new MediaType("application", "vnd.afpc.modca-pagesegment", Compressible, NotBinary)
      lazy val `vnd.ah-barcode`: MediaType                                  =
        new MediaType("application", "vnd.ah-barcode", Compressible, NotBinary)
      lazy val `vnd.ahead.space`: MediaType                                 =
        new MediaType("application", "vnd.ahead.space", Compressible, NotBinary, List("ahead"))
      lazy val `vnd.airzip.filesecure.azf`: MediaType                       = new MediaType(
        "application",
        "vnd.airzip.filesecure.azf",
        Compressible,
        NotBinary,
        List("azf"),
      )
      lazy val `vnd.airzip.filesecure.azs`: MediaType                       = new MediaType(
        "application",
        "vnd.airzip.filesecure.azs",
        Compressible,
        NotBinary,
        List("azs"),
      )
      lazy val `vnd.amadeus+json`: MediaType                                =
        new MediaType("application", "vnd.amadeus+json", Compressible, NotBinary)
      lazy val `vnd.amazon.ebook`: MediaType                                =
        new MediaType("application", "vnd.amazon.ebook", Compressible, NotBinary, List("azw"))
      lazy val `vnd.amazon.mobi8-ebook`: MediaType                          =
        new MediaType("application", "vnd.amazon.mobi8-ebook", Compressible, NotBinary)
      lazy val `vnd.americandynamics.acc`: MediaType                        = new MediaType(
        "application",
        "vnd.americandynamics.acc",
        Compressible,
        NotBinary,
        List("acc"),
      )
      lazy val `vnd.amiga.ami`: MediaType                                   =
        new MediaType("application", "vnd.amiga.ami", Compressible, NotBinary, List("ami"))
      lazy val `vnd.amundsen.maze+xml`: MediaType                           =
        new MediaType("application", "vnd.amundsen.maze+xml", Compressible, NotBinary)
      lazy val `vnd.android.ota`: MediaType                                 =
        new MediaType("application", "vnd.android.ota", Compressible, NotBinary)
      lazy val `vnd.android.package-archive`: MediaType                     = new MediaType(
        "application",
        "vnd.android.package-archive",
        Uncompressible,
        NotBinary,
        List("apk"),
      )
      lazy val `vnd.anki`: MediaType                                        =
        new MediaType("application", "vnd.anki", Compressible, NotBinary)
      lazy val `vnd.anser-web-certificate-issue-initiation`: MediaType      = new MediaType(
        "application",
        "vnd.anser-web-certificate-issue-initiation",
        Compressible,
        NotBinary,
        List("cii"),
      )
      lazy val `vnd.anser-web-funds-transfer-initiation`: MediaType         = new MediaType(
        "application",
        "vnd.anser-web-funds-transfer-initiation",
        Compressible,
        NotBinary,
        List("fti"),
      )
      lazy val `vnd.antix.game-component`: MediaType                        = new MediaType(
        "application",
        "vnd.antix.game-component",
        Compressible,
        NotBinary,
        List("atx"),
      )
      lazy val `vnd.apache.thrift.binary`: MediaType                        =
        new MediaType("application", "vnd.apache.thrift.binary", Compressible, NotBinary)
      lazy val `vnd.apache.thrift.compact`: MediaType                       =
        new MediaType("application", "vnd.apache.thrift.compact", Compressible, NotBinary)
      lazy val `vnd.apache.thrift.json`: MediaType                          =
        new MediaType("application", "vnd.apache.thrift.json", Compressible, NotBinary)
      lazy val `vnd.api+json`: MediaType                                    =
        new MediaType("application", "vnd.api+json", Compressible, Binary)
      lazy val `vnd.aplextor.warrp+json`: MediaType                         =
        new MediaType("application", "vnd.aplextor.warrp+json", Compressible, NotBinary)
      lazy val `vnd.apothekende.reservation+json`: MediaType                =
        new MediaType("application", "vnd.apothekende.reservation+json", Compressible, NotBinary)
      lazy val `vnd.apple.installer+xml`: MediaType                         = new MediaType(
        "application",
        "vnd.apple.installer+xml",
        Compressible,
        NotBinary,
        List("mpkg"),
      )
      lazy val `vnd.apple.keynote`: MediaType                               =
        new MediaType("application", "vnd.apple.keynote", Compressible, NotBinary, List("key"))
      lazy val `vnd.apple.mpegurl`: MediaType                               =
        new MediaType("application", "vnd.apple.mpegurl", Compressible, NotBinary, List("m3u8"))
      lazy val `vnd.apple.numbers`: MediaType                               =
        new MediaType("application", "vnd.apple.numbers", Compressible, NotBinary, List("numbers"))
      lazy val `vnd.apple.pages`: MediaType                                 =
        new MediaType("application", "vnd.apple.pages", Compressible, NotBinary, List("pages"))
      lazy val `vnd.apple.pkpass`: MediaType                                =
        new MediaType("application", "vnd.apple.pkpass", Uncompressible, NotBinary, List("pkpass"))
      lazy val `vnd.arastra.swi`: MediaType                                 =
        new MediaType("application", "vnd.arastra.swi", Compressible, NotBinary)
      lazy val `vnd.aristanetworks.swi`: MediaType                          =
        new MediaType("application", "vnd.aristanetworks.swi", Compressible, NotBinary, List("swi"))
      lazy val `vnd.artisan+json`: MediaType                                =
        new MediaType("application", "vnd.artisan+json", Compressible, NotBinary)
      lazy val `vnd.artsquare`: MediaType                                   =
        new MediaType("application", "vnd.artsquare", Compressible, NotBinary)
      lazy val `vnd.astraea-software.iota`: MediaType                       = new MediaType(
        "application",
        "vnd.astraea-software.iota",
        Compressible,
        NotBinary,
        List("iota"),
      )
      lazy val `vnd.audiograph`: MediaType                                  =
        new MediaType("application", "vnd.audiograph", Compressible, NotBinary, List("aep"))
      lazy val `vnd.autopackage`: MediaType                                 =
        new MediaType("application", "vnd.autopackage", Compressible, NotBinary)
      lazy val `vnd.avalon+json`: MediaType                                 =
        new MediaType("application", "vnd.avalon+json", Compressible, NotBinary)
      lazy val `vnd.avistar+xml`: MediaType                                 =
        new MediaType("application", "vnd.avistar+xml", Compressible, NotBinary)
      lazy val `vnd.balsamiq.bmml+xml`: MediaType                           =
        new MediaType("application", "vnd.balsamiq.bmml+xml", Compressible, NotBinary, List("bmml"))
      lazy val `vnd.balsamiq.bmpr`: MediaType                               =
        new MediaType("application", "vnd.balsamiq.bmpr", Compressible, NotBinary)
      lazy val `vnd.banana-accounting`: MediaType                           =
        new MediaType("application", "vnd.banana-accounting", Compressible, NotBinary)
      lazy val `vnd.bbf.usp.error`: MediaType                               =
        new MediaType("application", "vnd.bbf.usp.error", Compressible, NotBinary)
      lazy val `vnd.bbf.usp.msg`: MediaType                                 =
        new MediaType("application", "vnd.bbf.usp.msg", Compressible, NotBinary)
      lazy val `vnd.bbf.usp.msg+json`: MediaType                            =
        new MediaType("application", "vnd.bbf.usp.msg+json", Compressible, NotBinary)
      lazy val `vnd.bekitzur-stech+json`: MediaType                         =
        new MediaType("application", "vnd.bekitzur-stech+json", Compressible, NotBinary)
      lazy val `vnd.bint.med-content`: MediaType                            =
        new MediaType("application", "vnd.bint.med-content", Compressible, NotBinary)
      lazy val `vnd.biopax.rdf+xml`: MediaType                              =
        new MediaType("application", "vnd.biopax.rdf+xml", Compressible, NotBinary)
      lazy val `vnd.blink-idb-value-wrapper`: MediaType                     =
        new MediaType("application", "vnd.blink-idb-value-wrapper", Compressible, NotBinary)
      lazy val `vnd.blueice.multipass`: MediaType                           =
        new MediaType("application", "vnd.blueice.multipass", Compressible, NotBinary, List("mpm"))
      lazy val `vnd.bluetooth.ep.oob`: MediaType                            =
        new MediaType("application", "vnd.bluetooth.ep.oob", Compressible, NotBinary)
      lazy val `vnd.bluetooth.le.oob`: MediaType                            =
        new MediaType("application", "vnd.bluetooth.le.oob", Compressible, NotBinary)
      lazy val `vnd.bmi`: MediaType                                         =
        new MediaType("application", "vnd.bmi", Compressible, NotBinary, List("bmi"))
      lazy val `vnd.bpf`: MediaType                                         =
        new MediaType("application", "vnd.bpf", Compressible, NotBinary)
      lazy val `vnd.bpf3`: MediaType                                        =
        new MediaType("application", "vnd.bpf3", Compressible, NotBinary)
      lazy val `vnd.businessobjects`: MediaType                             =
        new MediaType("application", "vnd.businessobjects", Compressible, NotBinary, List("rep"))
      lazy val `vnd.byu.uapi+json`: MediaType                               =
        new MediaType("application", "vnd.byu.uapi+json", Compressible, NotBinary)
      lazy val `vnd.cab-jscript`: MediaType                                 =
        new MediaType("application", "vnd.cab-jscript", Compressible, NotBinary)
      lazy val `vnd.canon-cpdl`: MediaType                                  =
        new MediaType("application", "vnd.canon-cpdl", Compressible, NotBinary)
      lazy val `vnd.canon-lips`: MediaType                                  =
        new MediaType("application", "vnd.canon-lips", Compressible, NotBinary)
      lazy val `vnd.capasystems-pg+json`: MediaType                         =
        new MediaType("application", "vnd.capasystems-pg+json", Compressible, NotBinary)
      lazy val `vnd.cendio.thinlinc.clientconf`: MediaType                  =
        new MediaType("application", "vnd.cendio.thinlinc.clientconf", Compressible, NotBinary)
      lazy val `vnd.century-systems.tcp_stream`: MediaType                  =
        new MediaType("application", "vnd.century-systems.tcp_stream", Compressible, NotBinary)
      lazy val `vnd.chemdraw+xml`: MediaType                                =
        new MediaType("application", "vnd.chemdraw+xml", Compressible, NotBinary, List("cdxml"))
      lazy val `vnd.chess-pgn`: MediaType                                   =
        new MediaType("application", "vnd.chess-pgn", Compressible, NotBinary)
      lazy val `vnd.chipnuts.karaoke-mmd`: MediaType                        = new MediaType(
        "application",
        "vnd.chipnuts.karaoke-mmd",
        Compressible,
        NotBinary,
        List("mmd"),
      )
      lazy val `vnd.ciedi`: MediaType                                       =
        new MediaType("application", "vnd.ciedi", Compressible, NotBinary)
      lazy val `vnd.cinderella`: MediaType                                  =
        new MediaType("application", "vnd.cinderella", Compressible, NotBinary, List("cdy"))
      lazy val `vnd.cirpack.isdn-ext`: MediaType                            =
        new MediaType("application", "vnd.cirpack.isdn-ext", Compressible, NotBinary)
      lazy val `vnd.citationstyles.style+xml`: MediaType                    = new MediaType(
        "application",
        "vnd.citationstyles.style+xml",
        Compressible,
        NotBinary,
        List("csl"),
      )
      lazy val `vnd.claymore`: MediaType                                    =
        new MediaType("application", "vnd.claymore", Compressible, NotBinary, List("cla"))
      lazy val `vnd.cloanto.rp9`: MediaType                                 =
        new MediaType("application", "vnd.cloanto.rp9", Compressible, NotBinary, List("rp9"))
      lazy val `vnd.clonk.c4group`: MediaType                               = new MediaType(
        "application",
        "vnd.clonk.c4group",
        Compressible,
        NotBinary,
        List("c4g", "c4d", "c4f", "c4p", "c4u"),
      )
      lazy val `vnd.cluetrust.cartomobile-config`: MediaType                = new MediaType(
        "application",
        "vnd.cluetrust.cartomobile-config",
        Compressible,
        NotBinary,
        List("c11amc"),
      )
      lazy val `vnd.cluetrust.cartomobile-config-pkg`: MediaType            = new MediaType(
        "application",
        "vnd.cluetrust.cartomobile-config-pkg",
        Compressible,
        NotBinary,
        List("c11amz"),
      )
      lazy val `vnd.coffeescript`: MediaType                                =
        new MediaType("application", "vnd.coffeescript", Compressible, NotBinary)
      lazy val `vnd.collabio.xodocuments.document`: MediaType               =
        new MediaType("application", "vnd.collabio.xodocuments.document", Compressible, NotBinary)
      lazy val `vnd.collabio.xodocuments.document-template`: MediaType      = new MediaType(
        "application",
        "vnd.collabio.xodocuments.document-template",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.collabio.xodocuments.presentation`: MediaType           = new MediaType(
        "application",
        "vnd.collabio.xodocuments.presentation",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.collabio.xodocuments.presentation-template`: MediaType  = new MediaType(
        "application",
        "vnd.collabio.xodocuments.presentation-template",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.collabio.xodocuments.spreadsheet`: MediaType            = new MediaType(
        "application",
        "vnd.collabio.xodocuments.spreadsheet",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.collabio.xodocuments.spreadsheet-template`: MediaType   = new MediaType(
        "application",
        "vnd.collabio.xodocuments.spreadsheet-template",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.collection+json`: MediaType                             =
        new MediaType("application", "vnd.collection+json", Compressible, NotBinary)
      lazy val `vnd.collection.doc+json`: MediaType                         =
        new MediaType("application", "vnd.collection.doc+json", Compressible, NotBinary)
      lazy val `vnd.collection.next+json`: MediaType                        =
        new MediaType("application", "vnd.collection.next+json", Compressible, NotBinary)
      lazy val `vnd.comicbook+zip`: MediaType                               =
        new MediaType("application", "vnd.comicbook+zip", Uncompressible, NotBinary)
      lazy val `vnd.comicbook-rar`: MediaType                               =
        new MediaType("application", "vnd.comicbook-rar", Compressible, NotBinary)
      lazy val `vnd.commerce-battelle`: MediaType                           =
        new MediaType("application", "vnd.commerce-battelle", Compressible, NotBinary)
      lazy val `vnd.commonspace`: MediaType                                 =
        new MediaType("application", "vnd.commonspace", Compressible, NotBinary, List("csp"))
      lazy val `vnd.contact.cmsg`: MediaType                                =
        new MediaType("application", "vnd.contact.cmsg", Compressible, NotBinary, List("cdbcmsg"))
      lazy val `vnd.coreos.ignition+json`: MediaType                        =
        new MediaType("application", "vnd.coreos.ignition+json", Compressible, NotBinary)
      lazy val `vnd.cosmocaller`: MediaType                                 =
        new MediaType("application", "vnd.cosmocaller", Compressible, NotBinary, List("cmc"))
      lazy val `vnd.crick.clicker`: MediaType                               =
        new MediaType("application", "vnd.crick.clicker", Compressible, NotBinary, List("clkx"))
      lazy val `vnd.crick.clicker.keyboard`: MediaType                      = new MediaType(
        "application",
        "vnd.crick.clicker.keyboard",
        Compressible,
        NotBinary,
        List("clkk"),
      )
      lazy val `vnd.crick.clicker.palette`: MediaType                       = new MediaType(
        "application",
        "vnd.crick.clicker.palette",
        Compressible,
        NotBinary,
        List("clkp"),
      )
      lazy val `vnd.crick.clicker.template`: MediaType                      = new MediaType(
        "application",
        "vnd.crick.clicker.template",
        Compressible,
        NotBinary,
        List("clkt"),
      )
      lazy val `vnd.crick.clicker.wordbank`: MediaType                      = new MediaType(
        "application",
        "vnd.crick.clicker.wordbank",
        Compressible,
        NotBinary,
        List("clkw"),
      )
      lazy val `vnd.criticaltools.wbs+xml`: MediaType                       = new MediaType(
        "application",
        "vnd.criticaltools.wbs+xml",
        Compressible,
        NotBinary,
        List("wbs"),
      )
      lazy val `vnd.cryptii.pipe+json`: MediaType                           =
        new MediaType("application", "vnd.cryptii.pipe+json", Compressible, NotBinary)
      lazy val `vnd.crypto-shade-file`: MediaType                           =
        new MediaType("application", "vnd.crypto-shade-file", Compressible, NotBinary)
      lazy val `vnd.cryptomator.encrypted`: MediaType                       =
        new MediaType("application", "vnd.cryptomator.encrypted", Compressible, NotBinary)
      lazy val `vnd.cryptomator.vault`: MediaType                           =
        new MediaType("application", "vnd.cryptomator.vault", Compressible, NotBinary)
      lazy val `vnd.ctc-posml`: MediaType                                   =
        new MediaType("application", "vnd.ctc-posml", Compressible, NotBinary, List("pml"))
      lazy val `vnd.ctct.ws+xml`: MediaType                                 =
        new MediaType("application", "vnd.ctct.ws+xml", Compressible, NotBinary)
      lazy val `vnd.cups-pdf`: MediaType                                    =
        new MediaType("application", "vnd.cups-pdf", Compressible, NotBinary)
      lazy val `vnd.cups-postscript`: MediaType                             =
        new MediaType("application", "vnd.cups-postscript", Compressible, NotBinary)
      lazy val `vnd.cups-ppd`: MediaType                                    =
        new MediaType("application", "vnd.cups-ppd", Compressible, NotBinary, List("ppd"))
      lazy val `vnd.cups-raster`: MediaType                                 =
        new MediaType("application", "vnd.cups-raster", Compressible, NotBinary)
      lazy val `vnd.cups-raw`: MediaType                                    =
        new MediaType("application", "vnd.cups-raw", Compressible, NotBinary)
      lazy val `vnd.curl`: MediaType                                        =
        new MediaType("application", "vnd.curl", Compressible, NotBinary)
      lazy val `vnd.curl.car`: MediaType                                    =
        new MediaType("application", "vnd.curl.car", Compressible, NotBinary, List("car"))
      lazy val `vnd.curl.pcurl`: MediaType                                  =
        new MediaType("application", "vnd.curl.pcurl", Compressible, NotBinary, List("pcurl"))
      lazy val `vnd.cyan.dean.root+xml`: MediaType                          =
        new MediaType("application", "vnd.cyan.dean.root+xml", Compressible, NotBinary)
      lazy val `vnd.cybank`: MediaType                                      =
        new MediaType("application", "vnd.cybank", Compressible, NotBinary)
      lazy val `vnd.cyclonedx+json`: MediaType                              =
        new MediaType("application", "vnd.cyclonedx+json", Compressible, NotBinary)
      lazy val `vnd.cyclonedx+xml`: MediaType                               =
        new MediaType("application", "vnd.cyclonedx+xml", Compressible, NotBinary)
      lazy val `vnd.d2l.coursepackage1p0+zip`: MediaType                    =
        new MediaType("application", "vnd.d2l.coursepackage1p0+zip", Uncompressible, NotBinary)
      lazy val `vnd.d3m-dataset`: MediaType                                 =
        new MediaType("application", "vnd.d3m-dataset", Compressible, NotBinary)
      lazy val `vnd.d3m-problem`: MediaType                                 =
        new MediaType("application", "vnd.d3m-problem", Compressible, NotBinary)
      lazy val `vnd.dart`: MediaType                                        =
        new MediaType("application", "vnd.dart", Compressible, NotBinary, List("dart"))
      lazy val `vnd.data-vision.rdz`: MediaType                             =
        new MediaType("application", "vnd.data-vision.rdz", Compressible, NotBinary, List("rdz"))
      lazy val `vnd.datapackage+json`: MediaType                            =
        new MediaType("application", "vnd.datapackage+json", Compressible, NotBinary)
      lazy val `vnd.dataresource+json`: MediaType                           =
        new MediaType("application", "vnd.dataresource+json", Compressible, NotBinary)
      lazy val `vnd.dbf`: MediaType                                         =
        new MediaType("application", "vnd.dbf", Compressible, NotBinary, List("dbf"))
      lazy val `vnd.debian.binary-package`: MediaType                       =
        new MediaType("application", "vnd.debian.binary-package", Compressible, NotBinary)
      lazy val `vnd.dece.data`: MediaType                                   = new MediaType(
        "application",
        "vnd.dece.data",
        Compressible,
        NotBinary,
        List("uvf", "uvvf", "uvd", "uvvd"),
      )
      lazy val `vnd.dece.ttml+xml`: MediaType                               = new MediaType(
        "application",
        "vnd.dece.ttml+xml",
        Compressible,
        NotBinary,
        List("uvt", "uvvt"),
      )
      lazy val `vnd.dece.unspecified`: MediaType                            = new MediaType(
        "application",
        "vnd.dece.unspecified",
        Compressible,
        NotBinary,
        List("uvx", "uvvx"),
      )
      lazy val `vnd.dece.zip`: MediaType                                    =
        new MediaType("application", "vnd.dece.zip", Compressible, NotBinary, List("uvz", "uvvz"))
      lazy val `vnd.denovo.fcselayout-link`: MediaType                      = new MediaType(
        "application",
        "vnd.denovo.fcselayout-link",
        Compressible,
        NotBinary,
        List("fe_launch"),
      )
      lazy val `vnd.desmume.movie`: MediaType                               =
        new MediaType("application", "vnd.desmume.movie", Compressible, NotBinary)
      lazy val `vnd.dir-bi.plate-dl-nosuffix`: MediaType                    =
        new MediaType("application", "vnd.dir-bi.plate-dl-nosuffix", Compressible, NotBinary)
      lazy val `vnd.dm.delegation+xml`: MediaType                           =
        new MediaType("application", "vnd.dm.delegation+xml", Compressible, NotBinary)
      lazy val `vnd.dna`: MediaType                                         =
        new MediaType("application", "vnd.dna", Compressible, NotBinary, List("dna"))
      lazy val `vnd.document+json`: MediaType                               =
        new MediaType("application", "vnd.document+json", Compressible, NotBinary)
      lazy val `vnd.dolby.mlp`: MediaType                                   =
        new MediaType("application", "vnd.dolby.mlp", Compressible, NotBinary, List("mlp"))
      lazy val `vnd.dolby.mobile.1`: MediaType                              =
        new MediaType("application", "vnd.dolby.mobile.1", Compressible, NotBinary)
      lazy val `vnd.dolby.mobile.2`: MediaType                              =
        new MediaType("application", "vnd.dolby.mobile.2", Compressible, NotBinary)
      lazy val `vnd.doremir.scorecloud-binary-document`: MediaType          = new MediaType(
        "application",
        "vnd.doremir.scorecloud-binary-document",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.dpgraph`: MediaType                                     =
        new MediaType("application", "vnd.dpgraph", Compressible, NotBinary, List("dpg"))
      lazy val `vnd.dreamfactory`: MediaType                                =
        new MediaType("application", "vnd.dreamfactory", Compressible, NotBinary, List("dfac"))
      lazy val `vnd.drive+json`: MediaType                                  =
        new MediaType("application", "vnd.drive+json", Compressible, NotBinary)
      lazy val `vnd.ds-keypoint`: MediaType                                 =
        new MediaType("application", "vnd.ds-keypoint", Compressible, NotBinary, List("kpxx"))
      lazy val `vnd.dtg.local`: MediaType                                   =
        new MediaType("application", "vnd.dtg.local", Compressible, NotBinary)
      lazy val `vnd.dtg.local.flash`: MediaType                             =
        new MediaType("application", "vnd.dtg.local.flash", Compressible, NotBinary)
      lazy val `vnd.dtg.local.html`: MediaType                              =
        new MediaType("application", "vnd.dtg.local.html", Compressible, NotBinary)
      lazy val `vnd.dvb.ait`: MediaType                                     =
        new MediaType("application", "vnd.dvb.ait", Compressible, NotBinary, List("ait"))
      lazy val `vnd.dvb.dvbisl+xml`: MediaType                              =
        new MediaType("application", "vnd.dvb.dvbisl+xml", Compressible, NotBinary)
      lazy val `vnd.dvb.dvbj`: MediaType                                    =
        new MediaType("application", "vnd.dvb.dvbj", Compressible, NotBinary)
      lazy val `vnd.dvb.esgcontainer`: MediaType                            =
        new MediaType("application", "vnd.dvb.esgcontainer", Compressible, NotBinary)
      lazy val `vnd.dvb.ipdcdftnotifaccess`: MediaType                      =
        new MediaType("application", "vnd.dvb.ipdcdftnotifaccess", Compressible, NotBinary)
      lazy val `vnd.dvb.ipdcesgaccess`: MediaType                           =
        new MediaType("application", "vnd.dvb.ipdcesgaccess", Compressible, NotBinary)
      lazy val `vnd.dvb.ipdcesgaccess2`: MediaType                          =
        new MediaType("application", "vnd.dvb.ipdcesgaccess2", Compressible, NotBinary)
      lazy val `vnd.dvb.ipdcesgpdd`: MediaType                              =
        new MediaType("application", "vnd.dvb.ipdcesgpdd", Compressible, NotBinary)
      lazy val `vnd.dvb.ipdcroaming`: MediaType                             =
        new MediaType("application", "vnd.dvb.ipdcroaming", Compressible, NotBinary)
      lazy val `vnd.dvb.iptv.alfec-base`: MediaType                         =
        new MediaType("application", "vnd.dvb.iptv.alfec-base", Compressible, NotBinary)
      lazy val `vnd.dvb.iptv.alfec-enhancement`: MediaType                  =
        new MediaType("application", "vnd.dvb.iptv.alfec-enhancement", Compressible, NotBinary)
      lazy val `vnd.dvb.notif-aggregate-root+xml`: MediaType                =
        new MediaType("application", "vnd.dvb.notif-aggregate-root+xml", Compressible, NotBinary)
      lazy val `vnd.dvb.notif-container+xml`: MediaType                     =
        new MediaType("application", "vnd.dvb.notif-container+xml", Compressible, NotBinary)
      lazy val `vnd.dvb.notif-generic+xml`: MediaType                       =
        new MediaType("application", "vnd.dvb.notif-generic+xml", Compressible, NotBinary)
      lazy val `vnd.dvb.notif-ia-msglist+xml`: MediaType                    =
        new MediaType("application", "vnd.dvb.notif-ia-msglist+xml", Compressible, NotBinary)
      lazy val `vnd.dvb.notif-ia-registration-request+xml`: MediaType       = new MediaType(
        "application",
        "vnd.dvb.notif-ia-registration-request+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.dvb.notif-ia-registration-response+xml`: MediaType      = new MediaType(
        "application",
        "vnd.dvb.notif-ia-registration-response+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.dvb.notif-init+xml`: MediaType                          =
        new MediaType("application", "vnd.dvb.notif-init+xml", Compressible, NotBinary)
      lazy val `vnd.dvb.pfr`: MediaType                                     =
        new MediaType("application", "vnd.dvb.pfr", Compressible, NotBinary)
      lazy val `vnd.dvb.service`: MediaType                                 =
        new MediaType("application", "vnd.dvb.service", Compressible, NotBinary, List("svc"))
      lazy val `vnd.dxr`: MediaType                                         =
        new MediaType("application", "vnd.dxr", Compressible, NotBinary)
      lazy val `vnd.dynageo`: MediaType                                     =
        new MediaType("application", "vnd.dynageo", Compressible, NotBinary, List("geo"))
      lazy val `vnd.dzr`: MediaType                                         =
        new MediaType("application", "vnd.dzr", Compressible, NotBinary)
      lazy val `vnd.easykaraoke.cdgdownload`: MediaType                     =
        new MediaType("application", "vnd.easykaraoke.cdgdownload", Compressible, NotBinary)
      lazy val `vnd.ecdis-update`: MediaType                                =
        new MediaType("application", "vnd.ecdis-update", Compressible, NotBinary)
      lazy val `vnd.ecip.rlp`: MediaType                                    =
        new MediaType("application", "vnd.ecip.rlp", Compressible, NotBinary)
      lazy val `vnd.ecowin.chart`: MediaType                                =
        new MediaType("application", "vnd.ecowin.chart", Compressible, NotBinary, List("mag"))
      lazy val `vnd.ecowin.filerequest`: MediaType                          =
        new MediaType("application", "vnd.ecowin.filerequest", Compressible, NotBinary)
      lazy val `vnd.ecowin.fileupdate`: MediaType                           =
        new MediaType("application", "vnd.ecowin.fileupdate", Compressible, NotBinary)
      lazy val `vnd.ecowin.series`: MediaType                               =
        new MediaType("application", "vnd.ecowin.series", Compressible, NotBinary)
      lazy val `vnd.ecowin.seriesrequest`: MediaType                        =
        new MediaType("application", "vnd.ecowin.seriesrequest", Compressible, NotBinary)
      lazy val `vnd.ecowin.seriesupdate`: MediaType                         =
        new MediaType("application", "vnd.ecowin.seriesupdate", Compressible, NotBinary)
      lazy val `vnd.efi.img`: MediaType                                     =
        new MediaType("application", "vnd.efi.img", Compressible, NotBinary)
      lazy val `vnd.efi.iso`: MediaType                                     =
        new MediaType("application", "vnd.efi.iso", Compressible, NotBinary)
      lazy val `vnd.emclient.accessrequest+xml`: MediaType                  =
        new MediaType("application", "vnd.emclient.accessrequest+xml", Compressible, NotBinary)
      lazy val `vnd.enliven`: MediaType                                     =
        new MediaType("application", "vnd.enliven", Compressible, NotBinary, List("nml"))
      lazy val `vnd.enphase.envoy`: MediaType                               =
        new MediaType("application", "vnd.enphase.envoy", Compressible, NotBinary)
      lazy val `vnd.eprints.data+xml`: MediaType                            =
        new MediaType("application", "vnd.eprints.data+xml", Compressible, NotBinary)
      lazy val `vnd.epson.esf`: MediaType                                   =
        new MediaType("application", "vnd.epson.esf", Compressible, NotBinary, List("esf"))
      lazy val `vnd.epson.msf`: MediaType                                   =
        new MediaType("application", "vnd.epson.msf", Compressible, NotBinary, List("msf"))
      lazy val `vnd.epson.quickanime`: MediaType                            =
        new MediaType("application", "vnd.epson.quickanime", Compressible, NotBinary, List("qam"))
      lazy val `vnd.epson.salt`: MediaType                                  =
        new MediaType("application", "vnd.epson.salt", Compressible, NotBinary, List("slt"))
      lazy val `vnd.epson.ssf`: MediaType                                   =
        new MediaType("application", "vnd.epson.ssf", Compressible, NotBinary, List("ssf"))
      lazy val `vnd.ericsson.quickcall`: MediaType                          =
        new MediaType("application", "vnd.ericsson.quickcall", Compressible, NotBinary)
      lazy val `vnd.espass-espass+zip`: MediaType                           =
        new MediaType("application", "vnd.espass-espass+zip", Uncompressible, NotBinary)
      lazy val `vnd.eszigno3+xml`: MediaType                                = new MediaType(
        "application",
        "vnd.eszigno3+xml",
        Compressible,
        NotBinary,
        List("es3", "et3"),
      )
      lazy val `vnd.etsi.aoc+xml`: MediaType                                =
        new MediaType("application", "vnd.etsi.aoc+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.asic-e+zip`: MediaType                             =
        new MediaType("application", "vnd.etsi.asic-e+zip", Uncompressible, NotBinary)
      lazy val `vnd.etsi.asic-s+zip`: MediaType                             =
        new MediaType("application", "vnd.etsi.asic-s+zip", Uncompressible, NotBinary)
      lazy val `vnd.etsi.cug+xml`: MediaType                                =
        new MediaType("application", "vnd.etsi.cug+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.iptvcommand+xml`: MediaType                        =
        new MediaType("application", "vnd.etsi.iptvcommand+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.iptvdiscovery+xml`: MediaType                      =
        new MediaType("application", "vnd.etsi.iptvdiscovery+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.iptvprofile+xml`: MediaType                        =
        new MediaType("application", "vnd.etsi.iptvprofile+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.iptvsad-bc+xml`: MediaType                         =
        new MediaType("application", "vnd.etsi.iptvsad-bc+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.iptvsad-cod+xml`: MediaType                        =
        new MediaType("application", "vnd.etsi.iptvsad-cod+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.iptvsad-npvr+xml`: MediaType                       =
        new MediaType("application", "vnd.etsi.iptvsad-npvr+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.iptvservice+xml`: MediaType                        =
        new MediaType("application", "vnd.etsi.iptvservice+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.iptvsync+xml`: MediaType                           =
        new MediaType("application", "vnd.etsi.iptvsync+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.iptvueprofile+xml`: MediaType                      =
        new MediaType("application", "vnd.etsi.iptvueprofile+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.mcid+xml`: MediaType                               =
        new MediaType("application", "vnd.etsi.mcid+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.mheg5`: MediaType                                  =
        new MediaType("application", "vnd.etsi.mheg5", Compressible, NotBinary)
      lazy val `vnd.etsi.overload-control-policy-dataset+xml`: MediaType    = new MediaType(
        "application",
        "vnd.etsi.overload-control-policy-dataset+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.etsi.pstn+xml`: MediaType                               =
        new MediaType("application", "vnd.etsi.pstn+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.sci+xml`: MediaType                                =
        new MediaType("application", "vnd.etsi.sci+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.simservs+xml`: MediaType                           =
        new MediaType("application", "vnd.etsi.simservs+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.timestamp-token`: MediaType                        =
        new MediaType("application", "vnd.etsi.timestamp-token", Compressible, NotBinary)
      lazy val `vnd.etsi.tsl+xml`: MediaType                                =
        new MediaType("application", "vnd.etsi.tsl+xml", Compressible, NotBinary)
      lazy val `vnd.etsi.tsl.der`: MediaType                                =
        new MediaType("application", "vnd.etsi.tsl.der", Compressible, NotBinary)
      lazy val `vnd.eudora.data`: MediaType                                 =
        new MediaType("application", "vnd.eudora.data", Compressible, NotBinary)
      lazy val `vnd.evolv.ecig.profile`: MediaType                          =
        new MediaType("application", "vnd.evolv.ecig.profile", Compressible, NotBinary)
      lazy val `vnd.evolv.ecig.settings`: MediaType                         =
        new MediaType("application", "vnd.evolv.ecig.settings", Compressible, NotBinary)
      lazy val `vnd.evolv.ecig.theme`: MediaType                            =
        new MediaType("application", "vnd.evolv.ecig.theme", Compressible, NotBinary)
      lazy val `vnd.exstream-empower+zip`: MediaType                        =
        new MediaType("application", "vnd.exstream-empower+zip", Uncompressible, NotBinary)
      lazy val `vnd.exstream-package`: MediaType                            =
        new MediaType("application", "vnd.exstream-package", Compressible, NotBinary)
      lazy val `vnd.ezpix-album`: MediaType                                 =
        new MediaType("application", "vnd.ezpix-album", Compressible, NotBinary, List("ez2"))
      lazy val `vnd.ezpix-package`: MediaType                               =
        new MediaType("application", "vnd.ezpix-package", Compressible, NotBinary, List("ez3"))
      lazy val `vnd.f-secure.mobile`: MediaType                             =
        new MediaType("application", "vnd.f-secure.mobile", Compressible, NotBinary)
      lazy val `vnd.fastcopy-disk-image`: MediaType                         =
        new MediaType("application", "vnd.fastcopy-disk-image", Compressible, NotBinary)
      lazy val `vnd.fdf`: MediaType                                         =
        new MediaType("application", "vnd.fdf", Compressible, NotBinary, List("fdf"))
      lazy val `vnd.fdsn.mseed`: MediaType                                  =
        new MediaType("application", "vnd.fdsn.mseed", Compressible, NotBinary, List("mseed"))
      lazy val `vnd.fdsn.seed`: MediaType                                   = new MediaType(
        "application",
        "vnd.fdsn.seed",
        Compressible,
        NotBinary,
        List("seed", "dataless"),
      )
      lazy val `vnd.ffsns`: MediaType                                       =
        new MediaType("application", "vnd.ffsns", Compressible, NotBinary)
      lazy val `vnd.ficlab.flb+zip`: MediaType                              =
        new MediaType("application", "vnd.ficlab.flb+zip", Uncompressible, NotBinary)
      lazy val `vnd.filmit.zfc`: MediaType                                  =
        new MediaType("application", "vnd.filmit.zfc", Compressible, NotBinary)
      lazy val `vnd.fints`: MediaType                                       =
        new MediaType("application", "vnd.fints", Compressible, NotBinary)
      lazy val `vnd.firemonkeys.cloudcell`: MediaType                       =
        new MediaType("application", "vnd.firemonkeys.cloudcell", Compressible, NotBinary)
      lazy val `vnd.flographit`: MediaType                                  =
        new MediaType("application", "vnd.flographit", Compressible, NotBinary, List("gph"))
      lazy val `vnd.fluxtime.clip`: MediaType                               =
        new MediaType("application", "vnd.fluxtime.clip", Compressible, NotBinary, List("ftc"))
      lazy val `vnd.font-fontforge-sfd`: MediaType                          =
        new MediaType("application", "vnd.font-fontforge-sfd", Compressible, NotBinary)
      lazy val `vnd.framemaker`: MediaType                                  = new MediaType(
        "application",
        "vnd.framemaker",
        Compressible,
        NotBinary,
        List("fm", "frame", "maker", "book"),
      )
      lazy val `vnd.frogans.fnc`: MediaType                                 =
        new MediaType("application", "vnd.frogans.fnc", Compressible, NotBinary, List("fnc"))
      lazy val `vnd.frogans.ltf`: MediaType                                 =
        new MediaType("application", "vnd.frogans.ltf", Compressible, NotBinary, List("ltf"))
      lazy val `vnd.fsc.weblaunch`: MediaType                               =
        new MediaType("application", "vnd.fsc.weblaunch", Compressible, NotBinary, List("fsc"))
      lazy val `vnd.fujifilm.fb.docuworks`: MediaType                       =
        new MediaType("application", "vnd.fujifilm.fb.docuworks", Compressible, NotBinary)
      lazy val `vnd.fujifilm.fb.docuworks.binder`: MediaType                =
        new MediaType("application", "vnd.fujifilm.fb.docuworks.binder", Compressible, NotBinary)
      lazy val `vnd.fujifilm.fb.docuworks.container`: MediaType             =
        new MediaType("application", "vnd.fujifilm.fb.docuworks.container", Compressible, NotBinary)
      lazy val `vnd.fujifilm.fb.jfi+xml`: MediaType                         =
        new MediaType("application", "vnd.fujifilm.fb.jfi+xml", Compressible, NotBinary)
      lazy val `vnd.fujitsu.oasys`: MediaType                               =
        new MediaType("application", "vnd.fujitsu.oasys", Compressible, NotBinary, List("oas"))
      lazy val `vnd.fujitsu.oasys2`: MediaType                              =
        new MediaType("application", "vnd.fujitsu.oasys2", Compressible, NotBinary, List("oa2"))
      lazy val `vnd.fujitsu.oasys3`: MediaType                              =
        new MediaType("application", "vnd.fujitsu.oasys3", Compressible, NotBinary, List("oa3"))
      lazy val `vnd.fujitsu.oasysgp`: MediaType                             =
        new MediaType("application", "vnd.fujitsu.oasysgp", Compressible, NotBinary, List("fg5"))
      lazy val `vnd.fujitsu.oasysprs`: MediaType                            =
        new MediaType("application", "vnd.fujitsu.oasysprs", Compressible, NotBinary, List("bh2"))
      lazy val `vnd.fujixerox.art-ex`: MediaType                            =
        new MediaType("application", "vnd.fujixerox.art-ex", Compressible, NotBinary)
      lazy val `vnd.fujixerox.art4`: MediaType                              =
        new MediaType("application", "vnd.fujixerox.art4", Compressible, NotBinary)
      lazy val `vnd.fujixerox.ddd`: MediaType                               =
        new MediaType("application", "vnd.fujixerox.ddd", Compressible, NotBinary, List("ddd"))
      lazy val `vnd.fujixerox.docuworks`: MediaType                         = new MediaType(
        "application",
        "vnd.fujixerox.docuworks",
        Compressible,
        NotBinary,
        List("xdw"),
      )
      lazy val `vnd.fujixerox.docuworks.binder`: MediaType                  = new MediaType(
        "application",
        "vnd.fujixerox.docuworks.binder",
        Compressible,
        NotBinary,
        List("xbd"),
      )
      lazy val `vnd.fujixerox.docuworks.container`: MediaType               =
        new MediaType("application", "vnd.fujixerox.docuworks.container", Compressible, NotBinary)
      lazy val `vnd.fujixerox.hbpl`: MediaType                              =
        new MediaType("application", "vnd.fujixerox.hbpl", Compressible, NotBinary)
      lazy val `vnd.fut-misnet`: MediaType                                  =
        new MediaType("application", "vnd.fut-misnet", Compressible, NotBinary)
      lazy val `vnd.futoin+cbor`: MediaType                                 =
        new MediaType("application", "vnd.futoin+cbor", Compressible, NotBinary)
      lazy val `vnd.futoin+json`: MediaType                                 =
        new MediaType("application", "vnd.futoin+json", Compressible, NotBinary)
      lazy val `vnd.fuzzysheet`: MediaType                                  =
        new MediaType("application", "vnd.fuzzysheet", Compressible, NotBinary, List("fzs"))
      lazy val `vnd.genomatix.tuxedo`: MediaType                            =
        new MediaType("application", "vnd.genomatix.tuxedo", Compressible, NotBinary, List("txd"))
      lazy val `vnd.gentics.grd+json`: MediaType                            =
        new MediaType("application", "vnd.gentics.grd+json", Compressible, NotBinary)
      lazy val `vnd.geo+json`: MediaType                                    =
        new MediaType("application", "vnd.geo+json", Compressible, NotBinary)
      lazy val `vnd.geocube+xml`: MediaType                                 =
        new MediaType("application", "vnd.geocube+xml", Compressible, NotBinary)
      lazy val `vnd.geogebra.file`: MediaType                               =
        new MediaType("application", "vnd.geogebra.file", Compressible, NotBinary, List("ggb"))
      lazy val `vnd.geogebra.slides`: MediaType                             =
        new MediaType("application", "vnd.geogebra.slides", Compressible, NotBinary)
      lazy val `vnd.geogebra.tool`: MediaType                               =
        new MediaType("application", "vnd.geogebra.tool", Compressible, NotBinary, List("ggt"))
      lazy val `vnd.geometry-explorer`: MediaType                           = new MediaType(
        "application",
        "vnd.geometry-explorer",
        Compressible,
        NotBinary,
        List("gex", "gre"),
      )
      lazy val `vnd.geonext`: MediaType                                     =
        new MediaType("application", "vnd.geonext", Compressible, NotBinary, List("gxt"))
      lazy val `vnd.geoplan`: MediaType                                     =
        new MediaType("application", "vnd.geoplan", Compressible, NotBinary, List("g2w"))
      lazy val `vnd.geospace`: MediaType                                    =
        new MediaType("application", "vnd.geospace", Compressible, NotBinary, List("g3w"))
      lazy val `vnd.gerber`: MediaType                                      =
        new MediaType("application", "vnd.gerber", Compressible, NotBinary)
      lazy val `vnd.globalplatform.card-content-mgt`: MediaType             =
        new MediaType("application", "vnd.globalplatform.card-content-mgt", Compressible, NotBinary)
      lazy val `vnd.globalplatform.card-content-mgt-response`: MediaType    = new MediaType(
        "application",
        "vnd.globalplatform.card-content-mgt-response",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.gmx`: MediaType                                         =
        new MediaType("application", "vnd.gmx", Compressible, NotBinary, List("gmx"))
      lazy val `vnd.google-apps.document`: MediaType                        = new MediaType(
        "application",
        "vnd.google-apps.document",
        Uncompressible,
        NotBinary,
        List("gdoc"),
      )
      lazy val `vnd.google-apps.presentation`: MediaType                    = new MediaType(
        "application",
        "vnd.google-apps.presentation",
        Uncompressible,
        NotBinary,
        List("gslides"),
      )
      lazy val `vnd.google-apps.spreadsheet`: MediaType                     = new MediaType(
        "application",
        "vnd.google-apps.spreadsheet",
        Uncompressible,
        NotBinary,
        List("gsheet"),
      )
      lazy val `vnd.google-earth.kml+xml`: MediaType                        = new MediaType(
        "application",
        "vnd.google-earth.kml+xml",
        Compressible,
        NotBinary,
        List("kml"),
      )
      lazy val `vnd.google-earth.kmz`: MediaType                            =
        new MediaType("application", "vnd.google-earth.kmz", Uncompressible, Binary, List("kmz"))
      lazy val `vnd.gov.sk.e-form+xml`: MediaType                           =
        new MediaType("application", "vnd.gov.sk.e-form+xml", Compressible, NotBinary)
      lazy val `vnd.gov.sk.e-form+zip`: MediaType                           =
        new MediaType("application", "vnd.gov.sk.e-form+zip", Uncompressible, NotBinary)
      lazy val `vnd.gov.sk.xmldatacontainer+xml`: MediaType                 =
        new MediaType("application", "vnd.gov.sk.xmldatacontainer+xml", Compressible, NotBinary)
      lazy val `vnd.grafeq`: MediaType                                      =
        new MediaType("application", "vnd.grafeq", Compressible, NotBinary, List("gqf", "gqs"))
      lazy val `vnd.gridmp`: MediaType                                      =
        new MediaType("application", "vnd.gridmp", Compressible, NotBinary)
      lazy val `vnd.groove-account`: MediaType                              =
        new MediaType("application", "vnd.groove-account", Compressible, NotBinary, List("gac"))
      lazy val `vnd.groove-help`: MediaType                                 =
        new MediaType("application", "vnd.groove-help", Compressible, NotBinary, List("ghf"))
      lazy val `vnd.groove-identity-message`: MediaType                     = new MediaType(
        "application",
        "vnd.groove-identity-message",
        Compressible,
        NotBinary,
        List("gim"),
      )
      lazy val `vnd.groove-injector`: MediaType                             =
        new MediaType("application", "vnd.groove-injector", Compressible, NotBinary, List("grv"))
      lazy val `vnd.groove-tool-message`: MediaType                         = new MediaType(
        "application",
        "vnd.groove-tool-message",
        Compressible,
        NotBinary,
        List("gtm"),
      )
      lazy val `vnd.groove-tool-template`: MediaType                        = new MediaType(
        "application",
        "vnd.groove-tool-template",
        Compressible,
        NotBinary,
        List("tpl"),
      )
      lazy val `vnd.groove-vcard`: MediaType                                =
        new MediaType("application", "vnd.groove-vcard", Compressible, NotBinary, List("vcg"))
      lazy val `vnd.hal+json`: MediaType                                    =
        new MediaType("application", "vnd.hal+json", Compressible, NotBinary)
      lazy val `vnd.hal+xml`: MediaType                                     =
        new MediaType("application", "vnd.hal+xml", Compressible, NotBinary, List("hal"))
      lazy val `vnd.handheld-entertainment+xml`: MediaType                  = new MediaType(
        "application",
        "vnd.handheld-entertainment+xml",
        Compressible,
        NotBinary,
        List("zmm"),
      )
      lazy val `vnd.hbci`: MediaType                                        =
        new MediaType("application", "vnd.hbci", Compressible, NotBinary, List("hbci"))
      lazy val `vnd.hc+json`: MediaType                                     =
        new MediaType("application", "vnd.hc+json", Compressible, NotBinary)
      lazy val `vnd.hcl-bireports`: MediaType                               =
        new MediaType("application", "vnd.hcl-bireports", Compressible, NotBinary)
      lazy val `vnd.hdt`: MediaType                                         =
        new MediaType("application", "vnd.hdt", Compressible, NotBinary)
      lazy val `vnd.heroku+json`: MediaType                                 =
        new MediaType("application", "vnd.heroku+json", Compressible, NotBinary)
      lazy val `vnd.hhe.lesson-player`: MediaType                           =
        new MediaType("application", "vnd.hhe.lesson-player", Compressible, NotBinary, List("les"))
      lazy val `vnd.hp-hpgl`: MediaType                                     =
        new MediaType("application", "vnd.hp-hpgl", Compressible, NotBinary, List("hpgl"))
      lazy val `vnd.hp-hpid`: MediaType                                     =
        new MediaType("application", "vnd.hp-hpid", Compressible, NotBinary, List("hpid"))
      lazy val `vnd.hp-hps`: MediaType                                      =
        new MediaType("application", "vnd.hp-hps", Compressible, NotBinary, List("hps"))
      lazy val `vnd.hp-jlyt`: MediaType                                     =
        new MediaType("application", "vnd.hp-jlyt", Compressible, NotBinary, List("jlt"))
      lazy val `vnd.hp-pcl`: MediaType                                      =
        new MediaType("application", "vnd.hp-pcl", Compressible, NotBinary, List("pcl"))
      lazy val `vnd.hp-pclxl`: MediaType                                    =
        new MediaType("application", "vnd.hp-pclxl", Compressible, NotBinary, List("pclxl"))
      lazy val `vnd.httphone`: MediaType                                    =
        new MediaType("application", "vnd.httphone", Compressible, NotBinary)
      lazy val `vnd.hydrostatix.sof-data`: MediaType                        = new MediaType(
        "application",
        "vnd.hydrostatix.sof-data",
        Compressible,
        NotBinary,
        List("sfd-hdstx"),
      )
      lazy val `vnd.hyper+json`: MediaType                                  =
        new MediaType("application", "vnd.hyper+json", Compressible, NotBinary)
      lazy val `vnd.hyper-item+json`: MediaType                             =
        new MediaType("application", "vnd.hyper-item+json", Compressible, NotBinary)
      lazy val `vnd.hyperdrive+json`: MediaType                             =
        new MediaType("application", "vnd.hyperdrive+json", Compressible, NotBinary)
      lazy val `vnd.hzn-3d-crossword`: MediaType                            =
        new MediaType("application", "vnd.hzn-3d-crossword", Compressible, NotBinary)
      lazy val `vnd.ibm.afplinedata`: MediaType                             =
        new MediaType("application", "vnd.ibm.afplinedata", Compressible, NotBinary)
      lazy val `vnd.ibm.electronic-media`: MediaType                        =
        new MediaType("application", "vnd.ibm.electronic-media", Compressible, NotBinary)
      lazy val `vnd.ibm.minipay`: MediaType                                 =
        new MediaType("application", "vnd.ibm.minipay", Compressible, NotBinary, List("mpy"))
      lazy val `vnd.ibm.modcap`: MediaType                                  = new MediaType(
        "application",
        "vnd.ibm.modcap",
        Compressible,
        NotBinary,
        List("afp", "listafp", "list3820"),
      )
      lazy val `vnd.ibm.rights-management`: MediaType                       = new MediaType(
        "application",
        "vnd.ibm.rights-management",
        Compressible,
        NotBinary,
        List("irm"),
      )
      lazy val `vnd.ibm.secure-container`: MediaType                        = new MediaType(
        "application",
        "vnd.ibm.secure-container",
        Compressible,
        NotBinary,
        List("sc"),
      )
      lazy val `vnd.iccprofile`: MediaType                                  =
        new MediaType("application", "vnd.iccprofile", Compressible, NotBinary, List("icc", "icm"))
      lazy val `vnd.ieee.1905`: MediaType                                   =
        new MediaType("application", "vnd.ieee.1905", Compressible, NotBinary)
      lazy val `vnd.igloader`: MediaType                                    =
        new MediaType("application", "vnd.igloader", Compressible, NotBinary, List("igl"))
      lazy val `vnd.imagemeter.folder+zip`: MediaType                       =
        new MediaType("application", "vnd.imagemeter.folder+zip", Uncompressible, NotBinary)
      lazy val `vnd.imagemeter.image+zip`: MediaType                        =
        new MediaType("application", "vnd.imagemeter.image+zip", Uncompressible, NotBinary)
      lazy val `vnd.immervision-ivp`: MediaType                             =
        new MediaType("application", "vnd.immervision-ivp", Compressible, NotBinary, List("ivp"))
      lazy val `vnd.immervision-ivu`: MediaType                             =
        new MediaType("application", "vnd.immervision-ivu", Compressible, NotBinary, List("ivu"))
      lazy val `vnd.ims.imsccv1p1`: MediaType                               =
        new MediaType("application", "vnd.ims.imsccv1p1", Compressible, NotBinary)
      lazy val `vnd.ims.imsccv1p2`: MediaType                               =
        new MediaType("application", "vnd.ims.imsccv1p2", Compressible, NotBinary)
      lazy val `vnd.ims.imsccv1p3`: MediaType                               =
        new MediaType("application", "vnd.ims.imsccv1p3", Compressible, NotBinary)
      lazy val `vnd.ims.lis.v2.result+json`: MediaType                      =
        new MediaType("application", "vnd.ims.lis.v2.result+json", Compressible, NotBinary)
      lazy val `vnd.ims.lti.v2.toolconsumerprofile+json`: MediaType         = new MediaType(
        "application",
        "vnd.ims.lti.v2.toolconsumerprofile+json",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.ims.lti.v2.toolproxy+json`: MediaType                   =
        new MediaType("application", "vnd.ims.lti.v2.toolproxy+json", Compressible, NotBinary)
      lazy val `vnd.ims.lti.v2.toolproxy.id+json`: MediaType                =
        new MediaType("application", "vnd.ims.lti.v2.toolproxy.id+json", Compressible, NotBinary)
      lazy val `vnd.ims.lti.v2.toolsettings+json`: MediaType                =
        new MediaType("application", "vnd.ims.lti.v2.toolsettings+json", Compressible, NotBinary)
      lazy val `vnd.ims.lti.v2.toolsettings.simple+json`: MediaType         = new MediaType(
        "application",
        "vnd.ims.lti.v2.toolsettings.simple+json",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.informedcontrol.rms+xml`: MediaType                     =
        new MediaType("application", "vnd.informedcontrol.rms+xml", Compressible, NotBinary)
      lazy val `vnd.informix-visionary`: MediaType                          =
        new MediaType("application", "vnd.informix-visionary", Compressible, NotBinary)
      lazy val `vnd.infotech.project`: MediaType                            =
        new MediaType("application", "vnd.infotech.project", Compressible, NotBinary)
      lazy val `vnd.infotech.project+xml`: MediaType                        =
        new MediaType("application", "vnd.infotech.project+xml", Compressible, NotBinary)
      lazy val `vnd.innopath.wamp.notification`: MediaType                  =
        new MediaType("application", "vnd.innopath.wamp.notification", Compressible, NotBinary)
      lazy val `vnd.insors.igm`: MediaType                                  =
        new MediaType("application", "vnd.insors.igm", Compressible, NotBinary, List("igm"))
      lazy val `vnd.intercon.formnet`: MediaType                            = new MediaType(
        "application",
        "vnd.intercon.formnet",
        Compressible,
        NotBinary,
        List("xpw", "xpx"),
      )
      lazy val `vnd.intergeo`: MediaType                                    =
        new MediaType("application", "vnd.intergeo", Compressible, NotBinary, List("i2g"))
      lazy val `vnd.intertrust.digibox`: MediaType                          =
        new MediaType("application", "vnd.intertrust.digibox", Compressible, NotBinary)
      lazy val `vnd.intertrust.nncp`: MediaType                             =
        new MediaType("application", "vnd.intertrust.nncp", Compressible, NotBinary)
      lazy val `vnd.intu.qbo`: MediaType                                    =
        new MediaType("application", "vnd.intu.qbo", Compressible, NotBinary, List("qbo"))
      lazy val `vnd.intu.qfx`: MediaType                                    =
        new MediaType("application", "vnd.intu.qfx", Compressible, NotBinary, List("qfx"))
      lazy val `vnd.iptc.g2.catalogitem+xml`: MediaType                     =
        new MediaType("application", "vnd.iptc.g2.catalogitem+xml", Compressible, NotBinary)
      lazy val `vnd.iptc.g2.conceptitem+xml`: MediaType                     =
        new MediaType("application", "vnd.iptc.g2.conceptitem+xml", Compressible, NotBinary)
      lazy val `vnd.iptc.g2.knowledgeitem+xml`: MediaType                   =
        new MediaType("application", "vnd.iptc.g2.knowledgeitem+xml", Compressible, NotBinary)
      lazy val `vnd.iptc.g2.newsitem+xml`: MediaType                        =
        new MediaType("application", "vnd.iptc.g2.newsitem+xml", Compressible, NotBinary)
      lazy val `vnd.iptc.g2.newsmessage+xml`: MediaType                     =
        new MediaType("application", "vnd.iptc.g2.newsmessage+xml", Compressible, NotBinary)
      lazy val `vnd.iptc.g2.packageitem+xml`: MediaType                     =
        new MediaType("application", "vnd.iptc.g2.packageitem+xml", Compressible, NotBinary)
      lazy val `vnd.iptc.g2.planningitem+xml`: MediaType                    =
        new MediaType("application", "vnd.iptc.g2.planningitem+xml", Compressible, NotBinary)
      lazy val `vnd.ipunplugged.rcprofile`: MediaType                       = new MediaType(
        "application",
        "vnd.ipunplugged.rcprofile",
        Compressible,
        NotBinary,
        List("rcprofile"),
      )
      lazy val `vnd.irepository.package+xml`: MediaType                     = new MediaType(
        "application",
        "vnd.irepository.package+xml",
        Compressible,
        NotBinary,
        List("irp"),
      )
      lazy val `vnd.is-xpr`: MediaType                                      =
        new MediaType("application", "vnd.is-xpr", Compressible, NotBinary, List("xpr"))
      lazy val `vnd.isac.fcs`: MediaType                                    =
        new MediaType("application", "vnd.isac.fcs", Compressible, NotBinary, List("fcs"))
      lazy val `vnd.iso11783-10+zip`: MediaType                             =
        new MediaType("application", "vnd.iso11783-10+zip", Uncompressible, NotBinary)
      lazy val `vnd.jam`: MediaType                                         =
        new MediaType("application", "vnd.jam", Compressible, NotBinary, List("jam"))
      lazy val `vnd.japannet-directory-service`: MediaType                  =
        new MediaType("application", "vnd.japannet-directory-service", Compressible, NotBinary)
      lazy val `vnd.japannet-jpnstore-wakeup`: MediaType                    =
        new MediaType("application", "vnd.japannet-jpnstore-wakeup", Compressible, NotBinary)
      lazy val `vnd.japannet-payment-wakeup`: MediaType                     =
        new MediaType("application", "vnd.japannet-payment-wakeup", Compressible, NotBinary)
      lazy val `vnd.japannet-registration`: MediaType                       =
        new MediaType("application", "vnd.japannet-registration", Compressible, NotBinary)
      lazy val `vnd.japannet-registration-wakeup`: MediaType                =
        new MediaType("application", "vnd.japannet-registration-wakeup", Compressible, NotBinary)
      lazy val `vnd.japannet-setstore-wakeup`: MediaType                    =
        new MediaType("application", "vnd.japannet-setstore-wakeup", Compressible, NotBinary)
      lazy val `vnd.japannet-verification`: MediaType                       =
        new MediaType("application", "vnd.japannet-verification", Compressible, NotBinary)
      lazy val `vnd.japannet-verification-wakeup`: MediaType                =
        new MediaType("application", "vnd.japannet-verification-wakeup", Compressible, NotBinary)
      lazy val `vnd.jcp.javame.midlet-rms`: MediaType                       = new MediaType(
        "application",
        "vnd.jcp.javame.midlet-rms",
        Compressible,
        NotBinary,
        List("rms"),
      )
      lazy val `vnd.jisp`: MediaType                                        =
        new MediaType("application", "vnd.jisp", Compressible, NotBinary, List("jisp"))
      lazy val `vnd.joost.joda-archive`: MediaType                          = new MediaType(
        "application",
        "vnd.joost.joda-archive",
        Compressible,
        NotBinary,
        List("joda"),
      )
      lazy val `vnd.jsk.isdn-ngn`: MediaType                                =
        new MediaType("application", "vnd.jsk.isdn-ngn", Compressible, NotBinary)
      lazy val `vnd.kahootz`: MediaType                                     =
        new MediaType("application", "vnd.kahootz", Compressible, NotBinary, List("ktz", "ktr"))
      lazy val `vnd.kde.karbon`: MediaType                                  =
        new MediaType("application", "vnd.kde.karbon", Compressible, NotBinary, List("karbon"))
      lazy val `vnd.kde.kchart`: MediaType                                  =
        new MediaType("application", "vnd.kde.kchart", Compressible, NotBinary, List("chrt"))
      lazy val `vnd.kde.kformula`: MediaType                                =
        new MediaType("application", "vnd.kde.kformula", Compressible, NotBinary, List("kfo"))
      lazy val `vnd.kde.kivio`: MediaType                                   =
        new MediaType("application", "vnd.kde.kivio", Compressible, NotBinary, List("flw"))
      lazy val `vnd.kde.kontour`: MediaType                                 =
        new MediaType("application", "vnd.kde.kontour", Compressible, NotBinary, List("kon"))
      lazy val `vnd.kde.kpresenter`: MediaType                              = new MediaType(
        "application",
        "vnd.kde.kpresenter",
        Compressible,
        NotBinary,
        List("kpr", "kpt"),
      )
      lazy val `vnd.kde.kspread`: MediaType                                 =
        new MediaType("application", "vnd.kde.kspread", Compressible, NotBinary, List("ksp"))
      lazy val `vnd.kde.kword`: MediaType                                   =
        new MediaType("application", "vnd.kde.kword", Compressible, NotBinary, List("kwd", "kwt"))
      lazy val `vnd.kenameaapp`: MediaType                                  =
        new MediaType("application", "vnd.kenameaapp", Compressible, NotBinary, List("htke"))
      lazy val `vnd.kidspiration`: MediaType                                =
        new MediaType("application", "vnd.kidspiration", Compressible, NotBinary, List("kia"))
      lazy val `vnd.kinar`: MediaType                                       =
        new MediaType("application", "vnd.kinar", Compressible, NotBinary, List("kne", "knp"))
      lazy val `vnd.koan`: MediaType                                        = new MediaType(
        "application",
        "vnd.koan",
        Compressible,
        NotBinary,
        List("skp", "skd", "skt", "skm"),
      )
      lazy val `vnd.kodak-descriptor`: MediaType                            =
        new MediaType("application", "vnd.kodak-descriptor", Compressible, NotBinary, List("sse"))
      lazy val `vnd.las`: MediaType                                         =
        new MediaType("application", "vnd.las", Compressible, NotBinary)
      lazy val `vnd.las.las+json`: MediaType                                =
        new MediaType("application", "vnd.las.las+json", Compressible, NotBinary)
      lazy val `vnd.las.las+xml`: MediaType                                 =
        new MediaType("application", "vnd.las.las+xml", Compressible, NotBinary, List("lasxml"))
      lazy val `vnd.laszip`: MediaType                                      =
        new MediaType("application", "vnd.laszip", Compressible, NotBinary)
      lazy val `vnd.leap+json`: MediaType                                   =
        new MediaType("application", "vnd.leap+json", Compressible, NotBinary)
      lazy val `vnd.liberty-request+xml`: MediaType                         =
        new MediaType("application", "vnd.liberty-request+xml", Compressible, NotBinary)
      lazy val `vnd.llamagraphics.life-balance.desktop`: MediaType          = new MediaType(
        "application",
        "vnd.llamagraphics.life-balance.desktop",
        Compressible,
        NotBinary,
        List("lbd"),
      )
      lazy val `vnd.llamagraphics.life-balance.exchange+xml`: MediaType     = new MediaType(
        "application",
        "vnd.llamagraphics.life-balance.exchange+xml",
        Compressible,
        NotBinary,
        List("lbe"),
      )
      lazy val `vnd.logipipe.circuit+zip`: MediaType                        =
        new MediaType("application", "vnd.logipipe.circuit+zip", Uncompressible, NotBinary)
      lazy val `vnd.loom`: MediaType                                        =
        new MediaType("application", "vnd.loom", Compressible, NotBinary)
      lazy val `vnd.lotus-1-2-3`: MediaType                                 =
        new MediaType("application", "vnd.lotus-1-2-3", Compressible, NotBinary, List("123"))
      lazy val `vnd.lotus-approach`: MediaType                              =
        new MediaType("application", "vnd.lotus-approach", Compressible, NotBinary, List("apr"))
      lazy val `vnd.lotus-freelance`: MediaType                             =
        new MediaType("application", "vnd.lotus-freelance", Compressible, NotBinary, List("pre"))
      lazy val `vnd.lotus-notes`: MediaType                                 =
        new MediaType("application", "vnd.lotus-notes", Compressible, NotBinary, List("nsf"))
      lazy val `vnd.lotus-organizer`: MediaType                             =
        new MediaType("application", "vnd.lotus-organizer", Compressible, NotBinary, List("org"))
      lazy val `vnd.lotus-screencam`: MediaType                             =
        new MediaType("application", "vnd.lotus-screencam", Compressible, NotBinary, List("scm"))
      lazy val `vnd.lotus-wordpro`: MediaType                               =
        new MediaType("application", "vnd.lotus-wordpro", Compressible, NotBinary, List("lwp"))
      lazy val `vnd.macports.portpkg`: MediaType                            = new MediaType(
        "application",
        "vnd.macports.portpkg",
        Compressible,
        NotBinary,
        List("portpkg"),
      )
      lazy val `vnd.mapbox-vector-tile`: MediaType                          =
        new MediaType("application", "vnd.mapbox-vector-tile", Compressible, NotBinary, List("mvt"))
      lazy val `vnd.marlin.drm.actiontoken+xml`: MediaType                  =
        new MediaType("application", "vnd.marlin.drm.actiontoken+xml", Compressible, NotBinary)
      lazy val `vnd.marlin.drm.conftoken+xml`: MediaType                    =
        new MediaType("application", "vnd.marlin.drm.conftoken+xml", Compressible, NotBinary)
      lazy val `vnd.marlin.drm.license+xml`: MediaType                      =
        new MediaType("application", "vnd.marlin.drm.license+xml", Compressible, NotBinary)
      lazy val `vnd.marlin.drm.mdcf`: MediaType                             =
        new MediaType("application", "vnd.marlin.drm.mdcf", Compressible, NotBinary)
      lazy val `vnd.mason+json`: MediaType                                  =
        new MediaType("application", "vnd.mason+json", Compressible, NotBinary)
      lazy val `vnd.maxmind.maxmind-db`: MediaType                          =
        new MediaType("application", "vnd.maxmind.maxmind-db", Compressible, NotBinary)
      lazy val `vnd.mcd`: MediaType                                         =
        new MediaType("application", "vnd.mcd", Compressible, NotBinary, List("mcd"))
      lazy val `vnd.medcalcdata`: MediaType                                 =
        new MediaType("application", "vnd.medcalcdata", Compressible, NotBinary, List("mc1"))
      lazy val `vnd.mediastation.cdkey`: MediaType                          = new MediaType(
        "application",
        "vnd.mediastation.cdkey",
        Compressible,
        NotBinary,
        List("cdkey"),
      )
      lazy val `vnd.meridian-slingshot`: MediaType                          =
        new MediaType("application", "vnd.meridian-slingshot", Compressible, NotBinary)
      lazy val `vnd.mfer`: MediaType                                        =
        new MediaType("application", "vnd.mfer", Compressible, NotBinary, List("mwf"))
      lazy val `vnd.mfmp`: MediaType                                        =
        new MediaType("application", "vnd.mfmp", Compressible, NotBinary, List("mfm"))
      lazy val `vnd.micro+json`: MediaType                                  =
        new MediaType("application", "vnd.micro+json", Compressible, NotBinary)
      lazy val `vnd.micrografx.flo`: MediaType                              =
        new MediaType("application", "vnd.micrografx.flo", Compressible, NotBinary, List("flo"))
      lazy val `vnd.micrografx.igx`: MediaType                              =
        new MediaType("application", "vnd.micrografx.igx", Compressible, NotBinary, List("igx"))
      lazy val `vnd.microsoft.portable-executable`: MediaType               =
        new MediaType("application", "vnd.microsoft.portable-executable", Compressible, NotBinary)
      lazy val `vnd.microsoft.windows.thumbnail-cache`: MediaType           = new MediaType(
        "application",
        "vnd.microsoft.windows.thumbnail-cache",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.miele+json`: MediaType                                  =
        new MediaType("application", "vnd.miele+json", Compressible, NotBinary)
      lazy val `vnd.mif`: MediaType                                         =
        new MediaType("application", "vnd.mif", Compressible, NotBinary, List("mif"))
      lazy val `vnd.minisoft-hp3000-save`: MediaType                        =
        new MediaType("application", "vnd.minisoft-hp3000-save", Compressible, NotBinary)
      lazy val `vnd.mitsubishi.misty-guard.trustweb`: MediaType             =
        new MediaType("application", "vnd.mitsubishi.misty-guard.trustweb", Compressible, NotBinary)
      lazy val `vnd.mobius.daf`: MediaType                                  =
        new MediaType("application", "vnd.mobius.daf", Compressible, NotBinary, List("daf"))
      lazy val `vnd.mobius.dis`: MediaType                                  =
        new MediaType("application", "vnd.mobius.dis", Compressible, NotBinary, List("dis"))
      lazy val `vnd.mobius.mbk`: MediaType                                  =
        new MediaType("application", "vnd.mobius.mbk", Compressible, NotBinary, List("mbk"))
      lazy val `vnd.mobius.mqy`: MediaType                                  =
        new MediaType("application", "vnd.mobius.mqy", Compressible, NotBinary, List("mqy"))
      lazy val `vnd.mobius.msl`: MediaType                                  =
        new MediaType("application", "vnd.mobius.msl", Compressible, NotBinary, List("msl"))
      lazy val `vnd.mobius.plc`: MediaType                                  =
        new MediaType("application", "vnd.mobius.plc", Compressible, NotBinary, List("plc"))
      lazy val `vnd.mobius.txf`: MediaType                                  =
        new MediaType("application", "vnd.mobius.txf", Compressible, NotBinary, List("txf"))
      lazy val `vnd.mophun.application`: MediaType                          =
        new MediaType("application", "vnd.mophun.application", Compressible, NotBinary, List("mpn"))
      lazy val `vnd.mophun.certificate`: MediaType                          =
        new MediaType("application", "vnd.mophun.certificate", Compressible, NotBinary, List("mpc"))
      lazy val `vnd.motorola.flexsuite`: MediaType                          =
        new MediaType("application", "vnd.motorola.flexsuite", Compressible, NotBinary)
      lazy val `vnd.motorola.flexsuite.adsi`: MediaType                     =
        new MediaType("application", "vnd.motorola.flexsuite.adsi", Compressible, NotBinary)
      lazy val `vnd.motorola.flexsuite.fis`: MediaType                      =
        new MediaType("application", "vnd.motorola.flexsuite.fis", Compressible, NotBinary)
      lazy val `vnd.motorola.flexsuite.gotap`: MediaType                    =
        new MediaType("application", "vnd.motorola.flexsuite.gotap", Compressible, NotBinary)
      lazy val `vnd.motorola.flexsuite.kmr`: MediaType                      =
        new MediaType("application", "vnd.motorola.flexsuite.kmr", Compressible, NotBinary)
      lazy val `vnd.motorola.flexsuite.ttc`: MediaType                      =
        new MediaType("application", "vnd.motorola.flexsuite.ttc", Compressible, NotBinary)
      lazy val `vnd.motorola.flexsuite.wem`: MediaType                      =
        new MediaType("application", "vnd.motorola.flexsuite.wem", Compressible, NotBinary)
      lazy val `vnd.motorola.iprm`: MediaType                               =
        new MediaType("application", "vnd.motorola.iprm", Compressible, NotBinary)
      lazy val `vnd.mozilla.xul+xml`: MediaType                             =
        new MediaType("application", "vnd.mozilla.xul+xml", Compressible, NotBinary, List("xul"))
      lazy val `vnd.ms-3mfdocument`: MediaType                              =
        new MediaType("application", "vnd.ms-3mfdocument", Compressible, NotBinary)
      lazy val `vnd.ms-artgalry`: MediaType                                 =
        new MediaType("application", "vnd.ms-artgalry", Compressible, NotBinary, List("cil"))
      lazy val `vnd.ms-asf`: MediaType                                      =
        new MediaType("application", "vnd.ms-asf", Compressible, NotBinary)
      lazy val `vnd.ms-cab-compressed`: MediaType                           =
        new MediaType("application", "vnd.ms-cab-compressed", Compressible, NotBinary, List("cab"))
      lazy val `vnd.ms-color.iccprofile`: MediaType                         =
        new MediaType("application", "vnd.ms-color.iccprofile", Compressible, NotBinary)
      lazy val `vnd.ms-excel`: MediaType                                    = new MediaType(
        "application",
        "vnd.ms-excel",
        Uncompressible,
        NotBinary,
        List("xls", "xlm", "xla", "xlc", "xlt", "xlw"),
      )
      lazy val `vnd.ms-excel.addin.macroenabled.12`: MediaType              = new MediaType(
        "application",
        "vnd.ms-excel.addin.macroenabled.12",
        Compressible,
        NotBinary,
        List("xlam"),
      )
      lazy val `vnd.ms-excel.sheet.binary.macroenabled.12`: MediaType       = new MediaType(
        "application",
        "vnd.ms-excel.sheet.binary.macroenabled.12",
        Compressible,
        NotBinary,
        List("xlsb"),
      )
      lazy val `vnd.ms-excel.sheet.macroenabled.12`: MediaType              = new MediaType(
        "application",
        "vnd.ms-excel.sheet.macroenabled.12",
        Compressible,
        NotBinary,
        List("xlsm"),
      )
      lazy val `vnd.ms-excel.template.macroenabled.12`: MediaType           = new MediaType(
        "application",
        "vnd.ms-excel.template.macroenabled.12",
        Compressible,
        NotBinary,
        List("xltm"),
      )
      lazy val `vnd.ms-fontobject`: MediaType                               =
        new MediaType("application", "vnd.ms-fontobject", Compressible, Binary, List("eot"))
      lazy val `vnd.ms-htmlhelp`: MediaType                                 =
        new MediaType("application", "vnd.ms-htmlhelp", Compressible, NotBinary, List("chm"))
      lazy val `vnd.ms-ims`: MediaType                                      =
        new MediaType("application", "vnd.ms-ims", Compressible, NotBinary, List("ims"))
      lazy val `vnd.ms-lrm`: MediaType                                      =
        new MediaType("application", "vnd.ms-lrm", Compressible, NotBinary, List("lrm"))
      lazy val `vnd.ms-office.activex+xml`: MediaType                       =
        new MediaType("application", "vnd.ms-office.activex+xml", Compressible, NotBinary)
      lazy val `vnd.ms-officetheme`: MediaType                              =
        new MediaType("application", "vnd.ms-officetheme", Compressible, NotBinary, List("thmx"))
      lazy val `vnd.ms-opentype`: MediaType                                 =
        new MediaType("application", "vnd.ms-opentype", Compressible, NotBinary)
      lazy val `vnd.ms-outlook`: MediaType                                  =
        new MediaType("application", "vnd.ms-outlook", Uncompressible, NotBinary, List("msg"))
      lazy val `vnd.ms-package.obfuscated-opentype`: MediaType              =
        new MediaType("application", "vnd.ms-package.obfuscated-opentype", Compressible, NotBinary)
      lazy val `vnd.ms-pki.seccat`: MediaType                               =
        new MediaType("application", "vnd.ms-pki.seccat", Compressible, NotBinary, List("cat"))
      lazy val `vnd.ms-pki.stl`: MediaType                                  =
        new MediaType("application", "vnd.ms-pki.stl", Compressible, NotBinary, List("stl"))
      lazy val `vnd.ms-playready.initiator+xml`: MediaType                  =
        new MediaType("application", "vnd.ms-playready.initiator+xml", Compressible, NotBinary)
      lazy val `vnd.ms-powerpoint`: MediaType                               = new MediaType(
        "application",
        "vnd.ms-powerpoint",
        Uncompressible,
        NotBinary,
        List("ppt", "pps", "pot"),
      )
      lazy val `vnd.ms-powerpoint.addin.macroenabled.12`: MediaType         = new MediaType(
        "application",
        "vnd.ms-powerpoint.addin.macroenabled.12",
        Compressible,
        NotBinary,
        List("ppam"),
      )
      lazy val `vnd.ms-powerpoint.presentation.macroenabled.12`: MediaType  = new MediaType(
        "application",
        "vnd.ms-powerpoint.presentation.macroenabled.12",
        Compressible,
        NotBinary,
        List("pptm"),
      )
      lazy val `vnd.ms-powerpoint.slide.macroenabled.12`: MediaType         = new MediaType(
        "application",
        "vnd.ms-powerpoint.slide.macroenabled.12",
        Compressible,
        NotBinary,
        List("sldm"),
      )
      lazy val `vnd.ms-powerpoint.slideshow.macroenabled.12`: MediaType     = new MediaType(
        "application",
        "vnd.ms-powerpoint.slideshow.macroenabled.12",
        Compressible,
        NotBinary,
        List("ppsm"),
      )
      lazy val `vnd.ms-powerpoint.template.macroenabled.12`: MediaType      = new MediaType(
        "application",
        "vnd.ms-powerpoint.template.macroenabled.12",
        Compressible,
        NotBinary,
        List("potm"),
      )
      lazy val `vnd.ms-printdevicecapabilities+xml`: MediaType              =
        new MediaType("application", "vnd.ms-printdevicecapabilities+xml", Compressible, NotBinary)
      lazy val `vnd.ms-printing.printticket+xml`: MediaType                 =
        new MediaType("application", "vnd.ms-printing.printticket+xml", Compressible, NotBinary)
      lazy val `vnd.ms-printschematicket+xml`: MediaType                    =
        new MediaType("application", "vnd.ms-printschematicket+xml", Compressible, NotBinary)
      lazy val `vnd.ms-project`: MediaType                                  =
        new MediaType("application", "vnd.ms-project", Compressible, NotBinary, List("mpp", "mpt"))
      lazy val `vnd.ms-tnef`: MediaType                                     =
        new MediaType("application", "vnd.ms-tnef", Compressible, NotBinary)
      lazy val `vnd.ms-windows.devicepairing`: MediaType                    =
        new MediaType("application", "vnd.ms-windows.devicepairing", Compressible, NotBinary)
      lazy val `vnd.ms-windows.nwprinting.oob`: MediaType                   =
        new MediaType("application", "vnd.ms-windows.nwprinting.oob", Compressible, NotBinary)
      lazy val `vnd.ms-windows.printerpairing`: MediaType                   =
        new MediaType("application", "vnd.ms-windows.printerpairing", Compressible, NotBinary)
      lazy val `vnd.ms-windows.wsd.oob`: MediaType                          =
        new MediaType("application", "vnd.ms-windows.wsd.oob", Compressible, NotBinary)
      lazy val `vnd.ms-wmdrm.lic-chlg-req`: MediaType                       =
        new MediaType("application", "vnd.ms-wmdrm.lic-chlg-req", Compressible, NotBinary)
      lazy val part_1: List[MediaType]                                      = List(
        `vnd.adobe.air-application-installer-package+zip`,
        `vnd.adobe.flash.movie`,
        `vnd.adobe.formscentral.fcdt`,
        `vnd.adobe.fxp`,
        `vnd.adobe.partial-upload`,
        `vnd.adobe.xdp+xml`,
        `vnd.adobe.xfdf`,
        `vnd.aether.imp`,
        `vnd.afpc.afplinedata`,
        `vnd.afpc.afplinedata-pagedef`,
        `vnd.afpc.cmoca-cmresource`,
        `vnd.afpc.foca-charset`,
        `vnd.afpc.foca-codedfont`,
        `vnd.afpc.foca-codepage`,
        `vnd.afpc.modca`,
        `vnd.afpc.modca-cmtable`,
        `vnd.afpc.modca-formdef`,
        `vnd.afpc.modca-mediummap`,
        `vnd.afpc.modca-objectcontainer`,
        `vnd.afpc.modca-overlay`,
        `vnd.afpc.modca-pagesegment`,
        `vnd.ah-barcode`,
        `vnd.ahead.space`,
        `vnd.airzip.filesecure.azf`,
        `vnd.airzip.filesecure.azs`,
        `vnd.amadeus+json`,
        `vnd.amazon.ebook`,
        `vnd.amazon.mobi8-ebook`,
        `vnd.americandynamics.acc`,
        `vnd.amiga.ami`,
        `vnd.amundsen.maze+xml`,
        `vnd.android.ota`,
        `vnd.android.package-archive`,
        `vnd.anki`,
        `vnd.anser-web-certificate-issue-initiation`,
        `vnd.anser-web-funds-transfer-initiation`,
        `vnd.antix.game-component`,
        `vnd.apache.thrift.binary`,
        `vnd.apache.thrift.compact`,
        `vnd.apache.thrift.json`,
        `vnd.api+json`,
        `vnd.aplextor.warrp+json`,
        `vnd.apothekende.reservation+json`,
        `vnd.apple.installer+xml`,
        `vnd.apple.keynote`,
        `vnd.apple.mpegurl`,
        `vnd.apple.numbers`,
        `vnd.apple.pages`,
        `vnd.apple.pkpass`,
        `vnd.arastra.swi`,
        `vnd.aristanetworks.swi`,
        `vnd.artisan+json`,
        `vnd.artsquare`,
        `vnd.astraea-software.iota`,
        `vnd.audiograph`,
        `vnd.autopackage`,
        `vnd.avalon+json`,
        `vnd.avistar+xml`,
        `vnd.balsamiq.bmml+xml`,
        `vnd.balsamiq.bmpr`,
        `vnd.banana-accounting`,
        `vnd.bbf.usp.error`,
        `vnd.bbf.usp.msg`,
        `vnd.bbf.usp.msg+json`,
        `vnd.bekitzur-stech+json`,
        `vnd.bint.med-content`,
        `vnd.biopax.rdf+xml`,
        `vnd.blink-idb-value-wrapper`,
        `vnd.blueice.multipass`,
        `vnd.bluetooth.ep.oob`,
        `vnd.bluetooth.le.oob`,
        `vnd.bmi`,
        `vnd.bpf`,
        `vnd.bpf3`,
        `vnd.businessobjects`,
        `vnd.byu.uapi+json`,
        `vnd.cab-jscript`,
        `vnd.canon-cpdl`,
        `vnd.canon-lips`,
        `vnd.capasystems-pg+json`,
        `vnd.cendio.thinlinc.clientconf`,
        `vnd.century-systems.tcp_stream`,
        `vnd.chemdraw+xml`,
        `vnd.chess-pgn`,
        `vnd.chipnuts.karaoke-mmd`,
        `vnd.ciedi`,
        `vnd.cinderella`,
        `vnd.cirpack.isdn-ext`,
        `vnd.citationstyles.style+xml`,
        `vnd.claymore`,
        `vnd.cloanto.rp9`,
        `vnd.clonk.c4group`,
        `vnd.cluetrust.cartomobile-config`,
        `vnd.cluetrust.cartomobile-config-pkg`,
        `vnd.coffeescript`,
        `vnd.collabio.xodocuments.document`,
        `vnd.collabio.xodocuments.document-template`,
        `vnd.collabio.xodocuments.presentation`,
        `vnd.collabio.xodocuments.presentation-template`,
        `vnd.collabio.xodocuments.spreadsheet`,
        `vnd.collabio.xodocuments.spreadsheet-template`,
        `vnd.collection+json`,
        `vnd.collection.doc+json`,
        `vnd.collection.next+json`,
        `vnd.comicbook+zip`,
        `vnd.comicbook-rar`,
        `vnd.commerce-battelle`,
        `vnd.commonspace`,
        `vnd.contact.cmsg`,
        `vnd.coreos.ignition+json`,
        `vnd.cosmocaller`,
        `vnd.crick.clicker`,
        `vnd.crick.clicker.keyboard`,
        `vnd.crick.clicker.palette`,
        `vnd.crick.clicker.template`,
        `vnd.crick.clicker.wordbank`,
        `vnd.criticaltools.wbs+xml`,
        `vnd.cryptii.pipe+json`,
        `vnd.crypto-shade-file`,
        `vnd.cryptomator.encrypted`,
        `vnd.cryptomator.vault`,
        `vnd.ctc-posml`,
        `vnd.ctct.ws+xml`,
        `vnd.cups-pdf`,
        `vnd.cups-postscript`,
        `vnd.cups-ppd`,
        `vnd.cups-raster`,
        `vnd.cups-raw`,
        `vnd.curl`,
        `vnd.curl.car`,
        `vnd.curl.pcurl`,
        `vnd.cyan.dean.root+xml`,
        `vnd.cybank`,
        `vnd.cyclonedx+json`,
        `vnd.cyclonedx+xml`,
        `vnd.d2l.coursepackage1p0+zip`,
        `vnd.d3m-dataset`,
        `vnd.d3m-problem`,
        `vnd.dart`,
        `vnd.data-vision.rdz`,
        `vnd.datapackage+json`,
        `vnd.dataresource+json`,
        `vnd.dbf`,
        `vnd.debian.binary-package`,
        `vnd.dece.data`,
        `vnd.dece.ttml+xml`,
        `vnd.dece.unspecified`,
        `vnd.dece.zip`,
        `vnd.denovo.fcselayout-link`,
        `vnd.desmume.movie`,
        `vnd.dir-bi.plate-dl-nosuffix`,
        `vnd.dm.delegation+xml`,
        `vnd.dna`,
        `vnd.document+json`,
        `vnd.dolby.mlp`,
        `vnd.dolby.mobile.1`,
        `vnd.dolby.mobile.2`,
        `vnd.doremir.scorecloud-binary-document`,
        `vnd.dpgraph`,
        `vnd.dreamfactory`,
        `vnd.drive+json`,
        `vnd.ds-keypoint`,
        `vnd.dtg.local`,
        `vnd.dtg.local.flash`,
        `vnd.dtg.local.html`,
        `vnd.dvb.ait`,
        `vnd.dvb.dvbisl+xml`,
        `vnd.dvb.dvbj`,
        `vnd.dvb.esgcontainer`,
        `vnd.dvb.ipdcdftnotifaccess`,
        `vnd.dvb.ipdcesgaccess`,
        `vnd.dvb.ipdcesgaccess2`,
        `vnd.dvb.ipdcesgpdd`,
        `vnd.dvb.ipdcroaming`,
        `vnd.dvb.iptv.alfec-base`,
        `vnd.dvb.iptv.alfec-enhancement`,
        `vnd.dvb.notif-aggregate-root+xml`,
        `vnd.dvb.notif-container+xml`,
        `vnd.dvb.notif-generic+xml`,
        `vnd.dvb.notif-ia-msglist+xml`,
        `vnd.dvb.notif-ia-registration-request+xml`,
        `vnd.dvb.notif-ia-registration-response+xml`,
        `vnd.dvb.notif-init+xml`,
        `vnd.dvb.pfr`,
        `vnd.dvb.service`,
        `vnd.dxr`,
        `vnd.dynageo`,
        `vnd.dzr`,
        `vnd.easykaraoke.cdgdownload`,
        `vnd.ecdis-update`,
        `vnd.ecip.rlp`,
        `vnd.ecowin.chart`,
        `vnd.ecowin.filerequest`,
        `vnd.ecowin.fileupdate`,
        `vnd.ecowin.series`,
        `vnd.ecowin.seriesrequest`,
        `vnd.ecowin.seriesupdate`,
        `vnd.efi.img`,
        `vnd.efi.iso`,
        `vnd.emclient.accessrequest+xml`,
        `vnd.enliven`,
        `vnd.enphase.envoy`,
        `vnd.eprints.data+xml`,
        `vnd.epson.esf`,
        `vnd.epson.msf`,
        `vnd.epson.quickanime`,
        `vnd.epson.salt`,
        `vnd.epson.ssf`,
        `vnd.ericsson.quickcall`,
        `vnd.espass-espass+zip`,
        `vnd.eszigno3+xml`,
        `vnd.etsi.aoc+xml`,
        `vnd.etsi.asic-e+zip`,
        `vnd.etsi.asic-s+zip`,
        `vnd.etsi.cug+xml`,
        `vnd.etsi.iptvcommand+xml`,
        `vnd.etsi.iptvdiscovery+xml`,
        `vnd.etsi.iptvprofile+xml`,
        `vnd.etsi.iptvsad-bc+xml`,
        `vnd.etsi.iptvsad-cod+xml`,
        `vnd.etsi.iptvsad-npvr+xml`,
        `vnd.etsi.iptvservice+xml`,
        `vnd.etsi.iptvsync+xml`,
        `vnd.etsi.iptvueprofile+xml`,
        `vnd.etsi.mcid+xml`,
        `vnd.etsi.mheg5`,
        `vnd.etsi.overload-control-policy-dataset+xml`,
        `vnd.etsi.pstn+xml`,
        `vnd.etsi.sci+xml`,
        `vnd.etsi.simservs+xml`,
        `vnd.etsi.timestamp-token`,
        `vnd.etsi.tsl+xml`,
        `vnd.etsi.tsl.der`,
        `vnd.eudora.data`,
        `vnd.evolv.ecig.profile`,
        `vnd.evolv.ecig.settings`,
        `vnd.evolv.ecig.theme`,
        `vnd.exstream-empower+zip`,
        `vnd.exstream-package`,
        `vnd.ezpix-album`,
        `vnd.ezpix-package`,
        `vnd.f-secure.mobile`,
        `vnd.fastcopy-disk-image`,
        `vnd.fdf`,
        `vnd.fdsn.mseed`,
        `vnd.fdsn.seed`,
        `vnd.ffsns`,
        `vnd.ficlab.flb+zip`,
        `vnd.filmit.zfc`,
        `vnd.fints`,
        `vnd.firemonkeys.cloudcell`,
        `vnd.flographit`,
        `vnd.fluxtime.clip`,
        `vnd.font-fontforge-sfd`,
        `vnd.framemaker`,
        `vnd.frogans.fnc`,
        `vnd.frogans.ltf`,
        `vnd.fsc.weblaunch`,
        `vnd.fujifilm.fb.docuworks`,
        `vnd.fujifilm.fb.docuworks.binder`,
        `vnd.fujifilm.fb.docuworks.container`,
        `vnd.fujifilm.fb.jfi+xml`,
        `vnd.fujitsu.oasys`,
        `vnd.fujitsu.oasys2`,
        `vnd.fujitsu.oasys3`,
        `vnd.fujitsu.oasysgp`,
        `vnd.fujitsu.oasysprs`,
        `vnd.fujixerox.art-ex`,
        `vnd.fujixerox.art4`,
        `vnd.fujixerox.ddd`,
        `vnd.fujixerox.docuworks`,
        `vnd.fujixerox.docuworks.binder`,
        `vnd.fujixerox.docuworks.container`,
        `vnd.fujixerox.hbpl`,
        `vnd.fut-misnet`,
        `vnd.futoin+cbor`,
        `vnd.futoin+json`,
        `vnd.fuzzysheet`,
        `vnd.genomatix.tuxedo`,
        `vnd.gentics.grd+json`,
        `vnd.geo+json`,
        `vnd.geocube+xml`,
        `vnd.geogebra.file`,
        `vnd.geogebra.slides`,
        `vnd.geogebra.tool`,
        `vnd.geometry-explorer`,
        `vnd.geonext`,
        `vnd.geoplan`,
        `vnd.geospace`,
        `vnd.gerber`,
        `vnd.globalplatform.card-content-mgt`,
        `vnd.globalplatform.card-content-mgt-response`,
        `vnd.gmx`,
        `vnd.google-apps.document`,
        `vnd.google-apps.presentation`,
        `vnd.google-apps.spreadsheet`,
        `vnd.google-earth.kml+xml`,
        `vnd.google-earth.kmz`,
        `vnd.gov.sk.e-form+xml`,
        `vnd.gov.sk.e-form+zip`,
        `vnd.gov.sk.xmldatacontainer+xml`,
        `vnd.grafeq`,
        `vnd.gridmp`,
        `vnd.groove-account`,
        `vnd.groove-help`,
        `vnd.groove-identity-message`,
        `vnd.groove-injector`,
        `vnd.groove-tool-message`,
        `vnd.groove-tool-template`,
        `vnd.groove-vcard`,
        `vnd.hal+json`,
        `vnd.hal+xml`,
        `vnd.handheld-entertainment+xml`,
        `vnd.hbci`,
        `vnd.hc+json`,
        `vnd.hcl-bireports`,
        `vnd.hdt`,
        `vnd.heroku+json`,
        `vnd.hhe.lesson-player`,
        `vnd.hp-hpgl`,
        `vnd.hp-hpid`,
        `vnd.hp-hps`,
        `vnd.hp-jlyt`,
        `vnd.hp-pcl`,
        `vnd.hp-pclxl`,
        `vnd.httphone`,
        `vnd.hydrostatix.sof-data`,
        `vnd.hyper+json`,
        `vnd.hyper-item+json`,
        `vnd.hyperdrive+json`,
        `vnd.hzn-3d-crossword`,
        `vnd.ibm.afplinedata`,
        `vnd.ibm.electronic-media`,
        `vnd.ibm.minipay`,
        `vnd.ibm.modcap`,
        `vnd.ibm.rights-management`,
        `vnd.ibm.secure-container`,
        `vnd.iccprofile`,
        `vnd.ieee.1905`,
        `vnd.igloader`,
        `vnd.imagemeter.folder+zip`,
        `vnd.imagemeter.image+zip`,
        `vnd.immervision-ivp`,
        `vnd.immervision-ivu`,
        `vnd.ims.imsccv1p1`,
        `vnd.ims.imsccv1p2`,
        `vnd.ims.imsccv1p3`,
        `vnd.ims.lis.v2.result+json`,
        `vnd.ims.lti.v2.toolconsumerprofile+json`,
        `vnd.ims.lti.v2.toolproxy+json`,
        `vnd.ims.lti.v2.toolproxy.id+json`,
        `vnd.ims.lti.v2.toolsettings+json`,
        `vnd.ims.lti.v2.toolsettings.simple+json`,
        `vnd.informedcontrol.rms+xml`,
        `vnd.informix-visionary`,
        `vnd.infotech.project`,
        `vnd.infotech.project+xml`,
        `vnd.innopath.wamp.notification`,
        `vnd.insors.igm`,
        `vnd.intercon.formnet`,
        `vnd.intergeo`,
        `vnd.intertrust.digibox`,
        `vnd.intertrust.nncp`,
        `vnd.intu.qbo`,
        `vnd.intu.qfx`,
        `vnd.iptc.g2.catalogitem+xml`,
        `vnd.iptc.g2.conceptitem+xml`,
        `vnd.iptc.g2.knowledgeitem+xml`,
        `vnd.iptc.g2.newsitem+xml`,
        `vnd.iptc.g2.newsmessage+xml`,
        `vnd.iptc.g2.packageitem+xml`,
        `vnd.iptc.g2.planningitem+xml`,
        `vnd.ipunplugged.rcprofile`,
        `vnd.irepository.package+xml`,
        `vnd.is-xpr`,
        `vnd.isac.fcs`,
        `vnd.iso11783-10+zip`,
        `vnd.jam`,
        `vnd.japannet-directory-service`,
        `vnd.japannet-jpnstore-wakeup`,
        `vnd.japannet-payment-wakeup`,
        `vnd.japannet-registration`,
        `vnd.japannet-registration-wakeup`,
        `vnd.japannet-setstore-wakeup`,
        `vnd.japannet-verification`,
        `vnd.japannet-verification-wakeup`,
        `vnd.jcp.javame.midlet-rms`,
        `vnd.jisp`,
        `vnd.joost.joda-archive`,
        `vnd.jsk.isdn-ngn`,
        `vnd.kahootz`,
        `vnd.kde.karbon`,
        `vnd.kde.kchart`,
        `vnd.kde.kformula`,
        `vnd.kde.kivio`,
        `vnd.kde.kontour`,
        `vnd.kde.kpresenter`,
        `vnd.kde.kspread`,
        `vnd.kde.kword`,
        `vnd.kenameaapp`,
        `vnd.kidspiration`,
        `vnd.kinar`,
        `vnd.koan`,
        `vnd.kodak-descriptor`,
        `vnd.las`,
        `vnd.las.las+json`,
        `vnd.las.las+xml`,
        `vnd.laszip`,
        `vnd.leap+json`,
        `vnd.liberty-request+xml`,
        `vnd.llamagraphics.life-balance.desktop`,
        `vnd.llamagraphics.life-balance.exchange+xml`,
        `vnd.logipipe.circuit+zip`,
        `vnd.loom`,
        `vnd.lotus-1-2-3`,
        `vnd.lotus-approach`,
        `vnd.lotus-freelance`,
        `vnd.lotus-notes`,
        `vnd.lotus-organizer`,
        `vnd.lotus-screencam`,
        `vnd.lotus-wordpro`,
        `vnd.macports.portpkg`,
        `vnd.mapbox-vector-tile`,
        `vnd.marlin.drm.actiontoken+xml`,
        `vnd.marlin.drm.conftoken+xml`,
        `vnd.marlin.drm.license+xml`,
        `vnd.marlin.drm.mdcf`,
        `vnd.mason+json`,
        `vnd.maxmind.maxmind-db`,
        `vnd.mcd`,
        `vnd.medcalcdata`,
        `vnd.mediastation.cdkey`,
        `vnd.meridian-slingshot`,
        `vnd.mfer`,
        `vnd.mfmp`,
        `vnd.micro+json`,
        `vnd.micrografx.flo`,
        `vnd.micrografx.igx`,
        `vnd.microsoft.portable-executable`,
        `vnd.microsoft.windows.thumbnail-cache`,
        `vnd.miele+json`,
        `vnd.mif`,
        `vnd.minisoft-hp3000-save`,
        `vnd.mitsubishi.misty-guard.trustweb`,
        `vnd.mobius.daf`,
        `vnd.mobius.dis`,
        `vnd.mobius.mbk`,
        `vnd.mobius.mqy`,
        `vnd.mobius.msl`,
        `vnd.mobius.plc`,
        `vnd.mobius.txf`,
        `vnd.mophun.application`,
        `vnd.mophun.certificate`,
        `vnd.motorola.flexsuite`,
        `vnd.motorola.flexsuite.adsi`,
        `vnd.motorola.flexsuite.fis`,
        `vnd.motorola.flexsuite.gotap`,
        `vnd.motorola.flexsuite.kmr`,
        `vnd.motorola.flexsuite.ttc`,
        `vnd.motorola.flexsuite.wem`,
        `vnd.motorola.iprm`,
        `vnd.mozilla.xul+xml`,
        `vnd.ms-3mfdocument`,
        `vnd.ms-artgalry`,
        `vnd.ms-asf`,
        `vnd.ms-cab-compressed`,
        `vnd.ms-color.iccprofile`,
        `vnd.ms-excel`,
        `vnd.ms-excel.addin.macroenabled.12`,
        `vnd.ms-excel.sheet.binary.macroenabled.12`,
        `vnd.ms-excel.sheet.macroenabled.12`,
        `vnd.ms-excel.template.macroenabled.12`,
        `vnd.ms-fontobject`,
        `vnd.ms-htmlhelp`,
        `vnd.ms-ims`,
        `vnd.ms-lrm`,
        `vnd.ms-office.activex+xml`,
        `vnd.ms-officetheme`,
        `vnd.ms-opentype`,
        `vnd.ms-outlook`,
        `vnd.ms-package.obfuscated-opentype`,
        `vnd.ms-pki.seccat`,
        `vnd.ms-pki.stl`,
        `vnd.ms-playready.initiator+xml`,
        `vnd.ms-powerpoint`,
        `vnd.ms-powerpoint.addin.macroenabled.12`,
        `vnd.ms-powerpoint.presentation.macroenabled.12`,
        `vnd.ms-powerpoint.slide.macroenabled.12`,
        `vnd.ms-powerpoint.slideshow.macroenabled.12`,
        `vnd.ms-powerpoint.template.macroenabled.12`,
        `vnd.ms-printdevicecapabilities+xml`,
        `vnd.ms-printing.printticket+xml`,
        `vnd.ms-printschematicket+xml`,
        `vnd.ms-project`,
        `vnd.ms-tnef`,
        `vnd.ms-windows.devicepairing`,
        `vnd.ms-windows.nwprinting.oob`,
        `vnd.ms-windows.printerpairing`,
        `vnd.ms-windows.wsd.oob`,
        `vnd.ms-wmdrm.lic-chlg-req`,
      )
    }
    trait application_2 {
      lazy val `vnd.ms-wmdrm.lic-resp`: MediaType                                                    =
        new MediaType("application", "vnd.ms-wmdrm.lic-resp", Compressible, NotBinary)
      lazy val `vnd.ms-wmdrm.meter-chlg-req`: MediaType                                              =
        new MediaType("application", "vnd.ms-wmdrm.meter-chlg-req", Compressible, NotBinary)
      lazy val `vnd.ms-wmdrm.meter-resp`: MediaType                                                  =
        new MediaType("application", "vnd.ms-wmdrm.meter-resp", Compressible, NotBinary)
      lazy val `vnd.ms-word.document.macroenabled.12`: MediaType                                     = new MediaType(
        "application",
        "vnd.ms-word.document.macroenabled.12",
        Compressible,
        NotBinary,
        List("docm"),
      )
      lazy val `vnd.ms-word.template.macroenabled.12`: MediaType                                     = new MediaType(
        "application",
        "vnd.ms-word.template.macroenabled.12",
        Compressible,
        NotBinary,
        List("dotm"),
      )
      lazy val `vnd.ms-works`: MediaType                                                             = new MediaType(
        "application",
        "vnd.ms-works",
        Compressible,
        NotBinary,
        List("wps", "wks", "wcm", "wdb"),
      )
      lazy val `vnd.ms-wpl`: MediaType                                                               =
        new MediaType("application", "vnd.ms-wpl", Compressible, NotBinary, List("wpl"))
      lazy val `vnd.ms-xpsdocument`: MediaType                                                       =
        new MediaType("application", "vnd.ms-xpsdocument", Uncompressible, NotBinary, List("xps"))
      lazy val `vnd.msa-disk-image`: MediaType                                                       =
        new MediaType("application", "vnd.msa-disk-image", Compressible, NotBinary)
      lazy val `vnd.mseq`: MediaType                                                                 =
        new MediaType("application", "vnd.mseq", Compressible, NotBinary, List("mseq"))
      lazy val `vnd.msign`: MediaType                                                                =
        new MediaType("application", "vnd.msign", Compressible, NotBinary)
      lazy val `vnd.multiad.creator`: MediaType                                                      =
        new MediaType("application", "vnd.multiad.creator", Compressible, NotBinary)
      lazy val `vnd.multiad.creator.cif`: MediaType                                                  =
        new MediaType("application", "vnd.multiad.creator.cif", Compressible, NotBinary)
      lazy val `vnd.music-niff`: MediaType                                                           =
        new MediaType("application", "vnd.music-niff", Compressible, NotBinary)
      lazy val `vnd.musician`: MediaType                                                             =
        new MediaType("application", "vnd.musician", Compressible, NotBinary, List("mus"))
      lazy val `vnd.muvee.style`: MediaType                                                          =
        new MediaType("application", "vnd.muvee.style", Compressible, NotBinary, List("msty"))
      lazy val `vnd.mynfc`: MediaType                                                                =
        new MediaType("application", "vnd.mynfc", Compressible, NotBinary, List("taglet"))
      lazy val `vnd.ncd.control`: MediaType                                                          =
        new MediaType("application", "vnd.ncd.control", Compressible, NotBinary)
      lazy val `vnd.ncd.reference`: MediaType                                                        =
        new MediaType("application", "vnd.ncd.reference", Compressible, NotBinary)
      lazy val `vnd.nearst.inv+json`: MediaType                                                      =
        new MediaType("application", "vnd.nearst.inv+json", Compressible, NotBinary)
      lazy val `vnd.nebumind.line`: MediaType                                                        =
        new MediaType("application", "vnd.nebumind.line", Compressible, NotBinary)
      lazy val `vnd.nervana`: MediaType                                                              =
        new MediaType("application", "vnd.nervana", Compressible, NotBinary)
      lazy val `vnd.netfpx`: MediaType                                                               =
        new MediaType("application", "vnd.netfpx", Compressible, NotBinary)
      lazy val `vnd.neurolanguage.nlu`: MediaType                                                    =
        new MediaType("application", "vnd.neurolanguage.nlu", Compressible, NotBinary, List("nlu"))
      lazy val `vnd.nimn`: MediaType                                                                 =
        new MediaType("application", "vnd.nimn", Compressible, NotBinary)
      lazy val `vnd.nintendo.nitro.rom`: MediaType                                                   =
        new MediaType("application", "vnd.nintendo.nitro.rom", Compressible, NotBinary)
      lazy val `vnd.nintendo.snes.rom`: MediaType                                                    =
        new MediaType("application", "vnd.nintendo.snes.rom", Compressible, NotBinary)
      lazy val `vnd.nitf`: MediaType                                                                 =
        new MediaType("application", "vnd.nitf", Compressible, NotBinary, List("ntf", "nitf"))
      lazy val `vnd.noblenet-directory`: MediaType                                                   =
        new MediaType("application", "vnd.noblenet-directory", Compressible, NotBinary, List("nnd"))
      lazy val `vnd.noblenet-sealer`: MediaType                                                      =
        new MediaType("application", "vnd.noblenet-sealer", Compressible, NotBinary, List("nns"))
      lazy val `vnd.noblenet-web`: MediaType                                                         =
        new MediaType("application", "vnd.noblenet-web", Compressible, NotBinary, List("nnw"))
      lazy val `vnd.nokia.catalogs`: MediaType                                                       =
        new MediaType("application", "vnd.nokia.catalogs", Compressible, NotBinary)
      lazy val `vnd.nokia.conml+wbxml`: MediaType                                                    =
        new MediaType("application", "vnd.nokia.conml+wbxml", Compressible, NotBinary)
      lazy val `vnd.nokia.conml+xml`: MediaType                                                      =
        new MediaType("application", "vnd.nokia.conml+xml", Compressible, NotBinary)
      lazy val `vnd.nokia.iptv.config+xml`: MediaType                                                =
        new MediaType("application", "vnd.nokia.iptv.config+xml", Compressible, NotBinary)
      lazy val `vnd.nokia.isds-radio-presets`: MediaType                                             =
        new MediaType("application", "vnd.nokia.isds-radio-presets", Compressible, NotBinary)
      lazy val `vnd.nokia.landmark+wbxml`: MediaType                                                 =
        new MediaType("application", "vnd.nokia.landmark+wbxml", Compressible, NotBinary)
      lazy val `vnd.nokia.landmark+xml`: MediaType                                                   =
        new MediaType("application", "vnd.nokia.landmark+xml", Compressible, NotBinary)
      lazy val `vnd.nokia.landmarkcollection+xml`: MediaType                                         =
        new MediaType("application", "vnd.nokia.landmarkcollection+xml", Compressible, NotBinary)
      lazy val `vnd.nokia.n-gage.ac+xml`: MediaType                                                  =
        new MediaType("application", "vnd.nokia.n-gage.ac+xml", Compressible, NotBinary, List("ac"))
      lazy val `vnd.nokia.n-gage.data`: MediaType                                                    = new MediaType(
        "application",
        "vnd.nokia.n-gage.data",
        Compressible,
        NotBinary,
        List("ngdat"),
      )
      lazy val `vnd.nokia.n-gage.symbian.install`: MediaType                                         = new MediaType(
        "application",
        "vnd.nokia.n-gage.symbian.install",
        Compressible,
        NotBinary,
        List("n-gage"),
      )
      lazy val `vnd.nokia.ncd`: MediaType                                                            =
        new MediaType("application", "vnd.nokia.ncd", Compressible, NotBinary)
      lazy val `vnd.nokia.pcd+wbxml`: MediaType                                                      =
        new MediaType("application", "vnd.nokia.pcd+wbxml", Compressible, NotBinary)
      lazy val `vnd.nokia.pcd+xml`: MediaType                                                        =
        new MediaType("application", "vnd.nokia.pcd+xml", Compressible, NotBinary)
      lazy val `vnd.nokia.radio-preset`: MediaType                                                   = new MediaType(
        "application",
        "vnd.nokia.radio-preset",
        Compressible,
        NotBinary,
        List("rpst"),
      )
      lazy val `vnd.nokia.radio-presets`: MediaType                                                  = new MediaType(
        "application",
        "vnd.nokia.radio-presets",
        Compressible,
        NotBinary,
        List("rpss"),
      )
      lazy val `vnd.novadigm.edm`: MediaType                                                         =
        new MediaType("application", "vnd.novadigm.edm", Compressible, NotBinary, List("edm"))
      lazy val `vnd.novadigm.edx`: MediaType                                                         =
        new MediaType("application", "vnd.novadigm.edx", Compressible, NotBinary, List("edx"))
      lazy val `vnd.novadigm.ext`: MediaType                                                         =
        new MediaType("application", "vnd.novadigm.ext", Compressible, NotBinary, List("ext"))
      lazy val `vnd.ntt-local.content-share`: MediaType                                              =
        new MediaType("application", "vnd.ntt-local.content-share", Compressible, NotBinary)
      lazy val `vnd.ntt-local.file-transfer`: MediaType                                              =
        new MediaType("application", "vnd.ntt-local.file-transfer", Compressible, NotBinary)
      lazy val `vnd.ntt-local.ogw_remote-access`: MediaType                                          =
        new MediaType("application", "vnd.ntt-local.ogw_remote-access", Compressible, NotBinary)
      lazy val `vnd.ntt-local.sip-ta_remote`: MediaType                                              =
        new MediaType("application", "vnd.ntt-local.sip-ta_remote", Compressible, NotBinary)
      lazy val `vnd.ntt-local.sip-ta_tcp_stream`: MediaType                                          =
        new MediaType("application", "vnd.ntt-local.sip-ta_tcp_stream", Compressible, NotBinary)
      lazy val `vnd.oasis.opendocument.chart`: MediaType                                             = new MediaType(
        "application",
        "vnd.oasis.opendocument.chart",
        Compressible,
        Binary,
        List("odc"),
      )
      lazy val `vnd.oasis.opendocument.chart-template`: MediaType                                    = new MediaType(
        "application",
        "vnd.oasis.opendocument.chart-template",
        Compressible,
        NotBinary,
        List("otc"),
      )
      lazy val `vnd.oasis.opendocument.database`: MediaType                                          = new MediaType(
        "application",
        "vnd.oasis.opendocument.database",
        Compressible,
        Binary,
        List("odb"),
      )
      lazy val `vnd.oasis.opendocument.formula`: MediaType                                           = new MediaType(
        "application",
        "vnd.oasis.opendocument.formula",
        Compressible,
        Binary,
        List("odf"),
      )
      lazy val `vnd.oasis.opendocument.formula-template`: MediaType                                  = new MediaType(
        "application",
        "vnd.oasis.opendocument.formula-template",
        Compressible,
        NotBinary,
        List("odft"),
      )
      lazy val `vnd.oasis.opendocument.graphics`: MediaType                                          = new MediaType(
        "application",
        "vnd.oasis.opendocument.graphics",
        Uncompressible,
        Binary,
        List("odg"),
      )
      lazy val `vnd.oasis.opendocument.graphics-template`: MediaType                                 = new MediaType(
        "application",
        "vnd.oasis.opendocument.graphics-template",
        Compressible,
        NotBinary,
        List("otg"),
      )
      lazy val `vnd.oasis.opendocument.image`: MediaType                                             = new MediaType(
        "application",
        "vnd.oasis.opendocument.image",
        Compressible,
        Binary,
        List("odi"),
      )
      lazy val `vnd.oasis.opendocument.image-template`: MediaType                                    = new MediaType(
        "application",
        "vnd.oasis.opendocument.image-template",
        Compressible,
        NotBinary,
        List("oti"),
      )
      lazy val `vnd.oasis.opendocument.presentation`: MediaType                                      = new MediaType(
        "application",
        "vnd.oasis.opendocument.presentation",
        Uncompressible,
        Binary,
        List("odp"),
      )
      lazy val `vnd.oasis.opendocument.presentation-template`: MediaType                             = new MediaType(
        "application",
        "vnd.oasis.opendocument.presentation-template",
        Compressible,
        NotBinary,
        List("otp"),
      )
      lazy val `vnd.oasis.opendocument.spreadsheet`: MediaType                                       = new MediaType(
        "application",
        "vnd.oasis.opendocument.spreadsheet",
        Uncompressible,
        Binary,
        List("ods"),
      )
      lazy val `vnd.oasis.opendocument.spreadsheet-template`: MediaType                              = new MediaType(
        "application",
        "vnd.oasis.opendocument.spreadsheet-template",
        Compressible,
        NotBinary,
        List("ots"),
      )
      lazy val `vnd.oasis.opendocument.text`: MediaType                                              = new MediaType(
        "application",
        "vnd.oasis.opendocument.text",
        Uncompressible,
        Binary,
        List("odt"),
      )
      lazy val `vnd.oasis.opendocument.text-master`: MediaType                                       = new MediaType(
        "application",
        "vnd.oasis.opendocument.text-master",
        Compressible,
        Binary,
        List("odm"),
      )
      lazy val `vnd.oasis.opendocument.text-template`: MediaType                                     = new MediaType(
        "application",
        "vnd.oasis.opendocument.text-template",
        Compressible,
        NotBinary,
        List("ott"),
      )
      lazy val `vnd.oasis.opendocument.text-web`: MediaType                                          = new MediaType(
        "application",
        "vnd.oasis.opendocument.text-web",
        Compressible,
        Binary,
        List("oth"),
      )
      lazy val `vnd.obn`: MediaType                                                                  =
        new MediaType("application", "vnd.obn", Compressible, NotBinary)
      lazy val `vnd.ocf+cbor`: MediaType                                                             =
        new MediaType("application", "vnd.ocf+cbor", Compressible, NotBinary)
      lazy val `vnd.oci.image.manifest.v1+json`: MediaType                                           =
        new MediaType("application", "vnd.oci.image.manifest.v1+json", Compressible, NotBinary)
      lazy val `vnd.oftn.l10n+json`: MediaType                                                       =
        new MediaType("application", "vnd.oftn.l10n+json", Compressible, NotBinary)
      lazy val `vnd.oipf.contentaccessdownload+xml`: MediaType                                       =
        new MediaType("application", "vnd.oipf.contentaccessdownload+xml", Compressible, NotBinary)
      lazy val `vnd.oipf.contentaccessstreaming+xml`: MediaType                                      =
        new MediaType("application", "vnd.oipf.contentaccessstreaming+xml", Compressible, NotBinary)
      lazy val `vnd.oipf.cspg-hexbinary`: MediaType                                                  =
        new MediaType("application", "vnd.oipf.cspg-hexbinary", Compressible, NotBinary)
      lazy val `vnd.oipf.dae.svg+xml`: MediaType                                                     =
        new MediaType("application", "vnd.oipf.dae.svg+xml", Compressible, NotBinary)
      lazy val `vnd.oipf.dae.xhtml+xml`: MediaType                                                   =
        new MediaType("application", "vnd.oipf.dae.xhtml+xml", Compressible, NotBinary)
      lazy val `vnd.oipf.mippvcontrolmessage+xml`: MediaType                                         =
        new MediaType("application", "vnd.oipf.mippvcontrolmessage+xml", Compressible, NotBinary)
      lazy val `vnd.oipf.pae.gem`: MediaType                                                         =
        new MediaType("application", "vnd.oipf.pae.gem", Compressible, NotBinary)
      lazy val `vnd.oipf.spdiscovery+xml`: MediaType                                                 =
        new MediaType("application", "vnd.oipf.spdiscovery+xml", Compressible, NotBinary)
      lazy val `vnd.oipf.spdlist+xml`: MediaType                                                     =
        new MediaType("application", "vnd.oipf.spdlist+xml", Compressible, NotBinary)
      lazy val `vnd.oipf.ueprofile+xml`: MediaType                                                   =
        new MediaType("application", "vnd.oipf.ueprofile+xml", Compressible, NotBinary)
      lazy val `vnd.oipf.userprofile+xml`: MediaType                                                 =
        new MediaType("application", "vnd.oipf.userprofile+xml", Compressible, NotBinary)
      lazy val `vnd.olpc-sugar`: MediaType                                                           =
        new MediaType("application", "vnd.olpc-sugar", Compressible, NotBinary, List("xo"))
      lazy val `vnd.oma-scws-config`: MediaType                                                      =
        new MediaType("application", "vnd.oma-scws-config", Compressible, NotBinary)
      lazy val `vnd.oma-scws-http-request`: MediaType                                                =
        new MediaType("application", "vnd.oma-scws-http-request", Compressible, NotBinary)
      lazy val `vnd.oma-scws-http-response`: MediaType                                               =
        new MediaType("application", "vnd.oma-scws-http-response", Compressible, NotBinary)
      lazy val `vnd.oma.bcast.associated-procedure-parameter+xml`: MediaType                         = new MediaType(
        "application",
        "vnd.oma.bcast.associated-procedure-parameter+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.oma.bcast.drm-trigger+xml`: MediaType                                            =
        new MediaType("application", "vnd.oma.bcast.drm-trigger+xml", Compressible, NotBinary)
      lazy val `vnd.oma.bcast.imd+xml`: MediaType                                                    =
        new MediaType("application", "vnd.oma.bcast.imd+xml", Compressible, NotBinary)
      lazy val `vnd.oma.bcast.ltkm`: MediaType                                                       =
        new MediaType("application", "vnd.oma.bcast.ltkm", Compressible, NotBinary)
      lazy val `vnd.oma.bcast.notification+xml`: MediaType                                           =
        new MediaType("application", "vnd.oma.bcast.notification+xml", Compressible, NotBinary)
      lazy val `vnd.oma.bcast.provisioningtrigger`: MediaType                                        =
        new MediaType("application", "vnd.oma.bcast.provisioningtrigger", Compressible, NotBinary)
      lazy val `vnd.oma.bcast.sgboot`: MediaType                                                     =
        new MediaType("application", "vnd.oma.bcast.sgboot", Compressible, NotBinary)
      lazy val `vnd.oma.bcast.sgdd+xml`: MediaType                                                   =
        new MediaType("application", "vnd.oma.bcast.sgdd+xml", Compressible, NotBinary)
      lazy val `vnd.oma.bcast.sgdu`: MediaType                                                       =
        new MediaType("application", "vnd.oma.bcast.sgdu", Compressible, NotBinary)
      lazy val `vnd.oma.bcast.simple-symbol-container`: MediaType                                    = new MediaType(
        "application",
        "vnd.oma.bcast.simple-symbol-container",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.oma.bcast.smartcard-trigger+xml`: MediaType                                      =
        new MediaType("application", "vnd.oma.bcast.smartcard-trigger+xml", Compressible, NotBinary)
      lazy val `vnd.oma.bcast.sprov+xml`: MediaType                                                  =
        new MediaType("application", "vnd.oma.bcast.sprov+xml", Compressible, NotBinary)
      lazy val `vnd.oma.bcast.stkm`: MediaType                                                       =
        new MediaType("application", "vnd.oma.bcast.stkm", Compressible, NotBinary)
      lazy val `vnd.oma.cab-address-book+xml`: MediaType                                             =
        new MediaType("application", "vnd.oma.cab-address-book+xml", Compressible, NotBinary)
      lazy val `vnd.oma.cab-feature-handler+xml`: MediaType                                          =
        new MediaType("application", "vnd.oma.cab-feature-handler+xml", Compressible, NotBinary)
      lazy val `vnd.oma.cab-pcc+xml`: MediaType                                                      =
        new MediaType("application", "vnd.oma.cab-pcc+xml", Compressible, NotBinary)
      lazy val `vnd.oma.cab-subs-invite+xml`: MediaType                                              =
        new MediaType("application", "vnd.oma.cab-subs-invite+xml", Compressible, NotBinary)
      lazy val `vnd.oma.cab-user-prefs+xml`: MediaType                                               =
        new MediaType("application", "vnd.oma.cab-user-prefs+xml", Compressible, NotBinary)
      lazy val `vnd.oma.dcd`: MediaType                                                              =
        new MediaType("application", "vnd.oma.dcd", Compressible, NotBinary)
      lazy val `vnd.oma.dcdc`: MediaType                                                             =
        new MediaType("application", "vnd.oma.dcdc", Compressible, NotBinary)
      lazy val `vnd.oma.dd2+xml`: MediaType                                                          =
        new MediaType("application", "vnd.oma.dd2+xml", Compressible, NotBinary, List("dd2"))
      lazy val `vnd.oma.drm.risd+xml`: MediaType                                                     =
        new MediaType("application", "vnd.oma.drm.risd+xml", Compressible, NotBinary)
      lazy val `vnd.oma.group-usage-list+xml`: MediaType                                             =
        new MediaType("application", "vnd.oma.group-usage-list+xml", Compressible, NotBinary)
      lazy val `vnd.oma.lwm2m+cbor`: MediaType                                                       =
        new MediaType("application", "vnd.oma.lwm2m+cbor", Compressible, NotBinary)
      lazy val `vnd.oma.lwm2m+json`: MediaType                                                       =
        new MediaType("application", "vnd.oma.lwm2m+json", Compressible, NotBinary)
      lazy val `vnd.oma.lwm2m+tlv`: MediaType                                                        =
        new MediaType("application", "vnd.oma.lwm2m+tlv", Compressible, NotBinary)
      lazy val `vnd.oma.pal+xml`: MediaType                                                          =
        new MediaType("application", "vnd.oma.pal+xml", Compressible, NotBinary)
      lazy val `vnd.oma.poc.detailed-progress-report+xml`: MediaType                                 = new MediaType(
        "application",
        "vnd.oma.poc.detailed-progress-report+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.oma.poc.final-report+xml`: MediaType                                             =
        new MediaType("application", "vnd.oma.poc.final-report+xml", Compressible, NotBinary)
      lazy val `vnd.oma.poc.groups+xml`: MediaType                                                   =
        new MediaType("application", "vnd.oma.poc.groups+xml", Compressible, NotBinary)
      lazy val `vnd.oma.poc.invocation-descriptor+xml`: MediaType                                    = new MediaType(
        "application",
        "vnd.oma.poc.invocation-descriptor+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.oma.poc.optimized-progress-report+xml`: MediaType                                = new MediaType(
        "application",
        "vnd.oma.poc.optimized-progress-report+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.oma.push`: MediaType                                                             =
        new MediaType("application", "vnd.oma.push", Compressible, NotBinary)
      lazy val `vnd.oma.scidm.messages+xml`: MediaType                                               =
        new MediaType("application", "vnd.oma.scidm.messages+xml", Compressible, NotBinary)
      lazy val `vnd.oma.xcap-directory+xml`: MediaType                                               =
        new MediaType("application", "vnd.oma.xcap-directory+xml", Compressible, NotBinary)
      lazy val `vnd.omads-email+xml`: MediaType                                                      =
        new MediaType("application", "vnd.omads-email+xml", Compressible, NotBinary)
      lazy val `vnd.omads-file+xml`: MediaType                                                       =
        new MediaType("application", "vnd.omads-file+xml", Compressible, NotBinary)
      lazy val `vnd.omads-folder+xml`: MediaType                                                     =
        new MediaType("application", "vnd.omads-folder+xml", Compressible, NotBinary)
      lazy val `vnd.omaloc-supl-init`: MediaType                                                     =
        new MediaType("application", "vnd.omaloc-supl-init", Compressible, NotBinary)
      lazy val `vnd.onepager`: MediaType                                                             =
        new MediaType("application", "vnd.onepager", Compressible, NotBinary)
      lazy val `vnd.onepagertamp`: MediaType                                                         =
        new MediaType("application", "vnd.onepagertamp", Compressible, NotBinary)
      lazy val `vnd.onepagertamx`: MediaType                                                         =
        new MediaType("application", "vnd.onepagertamx", Compressible, NotBinary)
      lazy val `vnd.onepagertat`: MediaType                                                          =
        new MediaType("application", "vnd.onepagertat", Compressible, NotBinary)
      lazy val `vnd.onepagertatp`: MediaType                                                         =
        new MediaType("application", "vnd.onepagertatp", Compressible, NotBinary)
      lazy val `vnd.onepagertatx`: MediaType                                                         =
        new MediaType("application", "vnd.onepagertatx", Compressible, NotBinary)
      lazy val `vnd.openblox.game+xml`: MediaType                                                    =
        new MediaType("application", "vnd.openblox.game+xml", Compressible, NotBinary, List("obgx"))
      lazy val `vnd.openblox.game-binary`: MediaType                                                 =
        new MediaType("application", "vnd.openblox.game-binary", Compressible, NotBinary)
      lazy val `vnd.openeye.oeb`: MediaType                                                          =
        new MediaType("application", "vnd.openeye.oeb", Compressible, NotBinary)
      lazy val `vnd.openofficeorg.extension`: MediaType                                              = new MediaType(
        "application",
        "vnd.openofficeorg.extension",
        Compressible,
        NotBinary,
        List("oxt"),
      )
      lazy val `vnd.openstreetmap.data+xml`: MediaType                                               = new MediaType(
        "application",
        "vnd.openstreetmap.data+xml",
        Compressible,
        NotBinary,
        List("osm"),
      )
      lazy val `vnd.openxmlformats-officedocument.custom-properties+xml`: MediaType                  = new MediaType(
        "application",
        "vnd.openxmlformats-officedocument.custom-properties+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.openxmlformats-officedocument.customxmlproperties+xml`: MediaType                =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.customxmlproperties+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.drawing+xml`: MediaType                            = new MediaType(
        "application",
        "vnd.openxmlformats-officedocument.drawing+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.openxmlformats-officedocument.drawingml.chart+xml`: MediaType                    = new MediaType(
        "application",
        "vnd.openxmlformats-officedocument.drawingml.chart+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.openxmlformats-officedocument.drawingml.chartshapes+xml`: MediaType              =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.drawingml.chartshapes+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.drawingml.diagramcolors+xml`: MediaType            =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.drawingml.diagramcolors+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.drawingml.diagramdata+xml`: MediaType              =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.drawingml.diagramdata+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.drawingml.diagramlayout+xml`: MediaType            =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.drawingml.diagramlayout+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.drawingml.diagramstyle+xml`: MediaType             =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.drawingml.diagramstyle+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.extended-properties+xml`: MediaType                =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.extended-properties+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.commentauthors+xml`: MediaType      =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.commentauthors+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.comments+xml`: MediaType            =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.comments+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.handoutmaster+xml`: MediaType       =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.handoutmaster+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.notesmaster+xml`: MediaType         =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.notesmaster+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.notesslide+xml`: MediaType          =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.notesslide+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.presentation`: MediaType            =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.presentation",
          Uncompressible,
          Binary,
          List("pptx"),
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.presentation.main+xml`: MediaType   =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.presentation.main+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.presprops+xml`: MediaType           =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.presprops+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.slide`: MediaType                   = new MediaType(
        "application",
        "vnd.openxmlformats-officedocument.presentationml.slide",
        Compressible,
        Binary,
        List("sldx"),
      )
      lazy val `vnd.openxmlformats-officedocument.presentationml.slide+xml`: MediaType               =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.slide+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.slidelayout+xml`: MediaType         =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.slidelayout+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.slidemaster+xml`: MediaType         =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.slidemaster+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.slideshow`: MediaType               =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.slideshow",
          Compressible,
          Binary,
          List("ppsx"),
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.slideshow.main+xml`: MediaType      =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.slideshow.main+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.slideupdateinfo+xml`: MediaType     =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.slideupdateinfo+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.tablestyles+xml`: MediaType         =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.tablestyles+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.tags+xml`: MediaType                =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.tags+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.template`: MediaType                =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.template",
          Compressible,
          Binary,
          List("potx"),
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.template.main+xml`: MediaType       =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.template.main+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.presentationml.viewprops+xml`: MediaType           =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.presentationml.viewprops+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.calcchain+xml`: MediaType            =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.calcchain+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.chartsheet+xml`: MediaType           =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.chartsheet+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.comments+xml`: MediaType             =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.comments+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.connections+xml`: MediaType          =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.connections+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.dialogsheet+xml`: MediaType          =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.dialogsheet+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.externallink+xml`: MediaType         =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.externallink+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.pivotcachedefinition+xml`: MediaType = new MediaType(
        "application",
        "vnd.openxmlformats-officedocument.spreadsheetml.pivotcachedefinition+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.pivotcacherecords+xml`: MediaType    =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.pivotcacherecords+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.pivottable+xml`: MediaType           =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.pivottable+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.querytable+xml`: MediaType           =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.querytable+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.revisionheaders+xml`: MediaType      =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.revisionheaders+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.revisionlog+xml`: MediaType          =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.revisionlog+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.sharedstrings+xml`: MediaType        =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.sharedstrings+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.sheet`: MediaType                    = new MediaType(
        "application",
        "vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        Uncompressible,
        Binary,
        List("xlsx"),
      )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml`: MediaType           =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.sheetmetadata+xml`: MediaType        =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.sheetmetadata+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.styles+xml`: MediaType               =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.styles+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.table+xml`: MediaType                =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.table+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.tablesinglecells+xml`: MediaType     =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.tablesinglecells+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.template`: MediaType                 =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.template",
          Compressible,
          Binary,
          List("xltx"),
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.template.main+xml`: MediaType        =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.template.main+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.usernames+xml`: MediaType            =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.usernames+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.volatiledependencies+xml`: MediaType = new MediaType(
        "application",
        "vnd.openxmlformats-officedocument.spreadsheetml.volatiledependencies+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml`: MediaType            =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.theme+xml`: MediaType                              = new MediaType(
        "application",
        "vnd.openxmlformats-officedocument.theme+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.openxmlformats-officedocument.themeoverride+xml`: MediaType                      = new MediaType(
        "application",
        "vnd.openxmlformats-officedocument.themeoverride+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.openxmlformats-officedocument.vmldrawing`: MediaType                             = new MediaType(
        "application",
        "vnd.openxmlformats-officedocument.vmldrawing",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.comments+xml`: MediaType          =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.comments+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.document`: MediaType              =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.document",
          Uncompressible,
          Binary,
          List("docx"),
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.document.glossary+xml`: MediaType = new MediaType(
        "application",
        "vnd.openxmlformats-officedocument.wordprocessingml.document.glossary+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml`: MediaType     =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml`: MediaType          =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.fonttable+xml`: MediaType         =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.fonttable+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.footer+xml`: MediaType            =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.footer+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml`: MediaType         =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml`: MediaType         =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.settings+xml`: MediaType          =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.settings+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.styles+xml`: MediaType            =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.styles+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.template`: MediaType              =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.template",
          Compressible,
          Binary,
          List("dotx"),
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.template.main+xml`: MediaType     =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.template.main+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-officedocument.wordprocessingml.websettings+xml`: MediaType       =
        new MediaType(
          "application",
          "vnd.openxmlformats-officedocument.wordprocessingml.websettings+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-package.core-properties+xml`: MediaType                           = new MediaType(
        "application",
        "vnd.openxmlformats-package.core-properties+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.openxmlformats-package.digital-signature-xmlsignature+xml`: MediaType            =
        new MediaType(
          "application",
          "vnd.openxmlformats-package.digital-signature-xmlsignature+xml",
          Compressible,
          NotBinary,
        )
      lazy val `vnd.openxmlformats-package.relationships+xml`: MediaType                             = new MediaType(
        "application",
        "vnd.openxmlformats-package.relationships+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.oracle.resource+json`: MediaType                                                 =
        new MediaType("application", "vnd.oracle.resource+json", Compressible, NotBinary)
      lazy val `vnd.orange.indata`: MediaType                                                        =
        new MediaType("application", "vnd.orange.indata", Compressible, NotBinary)
      lazy val `vnd.osa.netdeploy`: MediaType                                                        =
        new MediaType("application", "vnd.osa.netdeploy", Compressible, NotBinary)
      lazy val `vnd.osgeo.mapguide.package`: MediaType                                               = new MediaType(
        "application",
        "vnd.osgeo.mapguide.package",
        Compressible,
        NotBinary,
        List("mgp"),
      )
      lazy val `vnd.osgi.bundle`: MediaType                                                          =
        new MediaType("application", "vnd.osgi.bundle", Compressible, NotBinary)
      lazy val `vnd.osgi.dp`: MediaType                                                              =
        new MediaType("application", "vnd.osgi.dp", Compressible, NotBinary, List("dp"))
      lazy val `vnd.osgi.subsystem`: MediaType                                                       =
        new MediaType("application", "vnd.osgi.subsystem", Compressible, NotBinary, List("esa"))
      lazy val `vnd.otps.ct-kip+xml`: MediaType                                                      =
        new MediaType("application", "vnd.otps.ct-kip+xml", Compressible, NotBinary)
      lazy val `vnd.oxli.countgraph`: MediaType                                                      =
        new MediaType("application", "vnd.oxli.countgraph", Compressible, NotBinary)
      lazy val `vnd.pagerduty+json`: MediaType                                                       =
        new MediaType("application", "vnd.pagerduty+json", Compressible, NotBinary)
      lazy val `vnd.palm`: MediaType                                                                 = new MediaType(
        "application",
        "vnd.palm",
        Compressible,
        NotBinary,
        List("pdb", "pqa", "oprc"),
      )
      lazy val `vnd.panoply`: MediaType                                                              =
        new MediaType("application", "vnd.panoply", Compressible, NotBinary)
      lazy val `vnd.paos.xml`: MediaType                                                             =
        new MediaType("application", "vnd.paos.xml", Compressible, NotBinary)
      lazy val `vnd.patentdive`: MediaType                                                           =
        new MediaType("application", "vnd.patentdive", Compressible, NotBinary)
      lazy val `vnd.patientecommsdoc`: MediaType                                                     =
        new MediaType("application", "vnd.patientecommsdoc", Compressible, NotBinary)
      lazy val `vnd.pawaafile`: MediaType                                                            =
        new MediaType("application", "vnd.pawaafile", Compressible, NotBinary, List("paw"))
      lazy val `vnd.pcos`: MediaType                                                                 =
        new MediaType("application", "vnd.pcos", Compressible, NotBinary)
      lazy val `vnd.pg.format`: MediaType                                                            =
        new MediaType("application", "vnd.pg.format", Compressible, NotBinary, List("str"))
      lazy val `vnd.pg.osasli`: MediaType                                                            =
        new MediaType("application", "vnd.pg.osasli", Compressible, NotBinary, List("ei6"))
      lazy val `vnd.piaccess.application-licence`: MediaType                                         =
        new MediaType("application", "vnd.piaccess.application-licence", Compressible, NotBinary)
      lazy val `vnd.picsel`: MediaType                                                               =
        new MediaType("application", "vnd.picsel", Compressible, NotBinary, List("efif"))
      lazy val `vnd.pmi.widget`: MediaType                                                           =
        new MediaType("application", "vnd.pmi.widget", Compressible, NotBinary, List("wg"))
      lazy val `vnd.poc.group-advertisement+xml`: MediaType                                          =
        new MediaType("application", "vnd.poc.group-advertisement+xml", Compressible, NotBinary)
      lazy val `vnd.pocketlearn`: MediaType                                                          =
        new MediaType("application", "vnd.pocketlearn", Compressible, NotBinary, List("plf"))
      lazy val `vnd.powerbuilder6`: MediaType                                                        =
        new MediaType("application", "vnd.powerbuilder6", Compressible, NotBinary, List("pbd"))
      lazy val `vnd.powerbuilder6-s`: MediaType                                                      =
        new MediaType("application", "vnd.powerbuilder6-s", Compressible, NotBinary)
      lazy val `vnd.powerbuilder7`: MediaType                                                        =
        new MediaType("application", "vnd.powerbuilder7", Compressible, NotBinary)
      lazy val `vnd.powerbuilder7-s`: MediaType                                                      =
        new MediaType("application", "vnd.powerbuilder7-s", Compressible, NotBinary)
      lazy val `vnd.powerbuilder75`: MediaType                                                       =
        new MediaType("application", "vnd.powerbuilder75", Compressible, NotBinary)
      lazy val `vnd.powerbuilder75-s`: MediaType                                                     =
        new MediaType("application", "vnd.powerbuilder75-s", Compressible, NotBinary)
      lazy val `vnd.preminet`: MediaType                                                             =
        new MediaType("application", "vnd.preminet", Compressible, NotBinary)
      lazy val `vnd.previewsystems.box`: MediaType                                                   =
        new MediaType("application", "vnd.previewsystems.box", Compressible, NotBinary, List("box"))
      lazy val `vnd.proteus.magazine`: MediaType                                                     =
        new MediaType("application", "vnd.proteus.magazine", Compressible, NotBinary, List("mgz"))
      lazy val `vnd.psfs`: MediaType                                                                 =
        new MediaType("application", "vnd.psfs", Compressible, NotBinary)
      lazy val `vnd.publishare-delta-tree`: MediaType                                                = new MediaType(
        "application",
        "vnd.publishare-delta-tree",
        Compressible,
        NotBinary,
        List("qps"),
      )
      lazy val `vnd.pvi.ptid1`: MediaType                                                            =
        new MediaType("application", "vnd.pvi.ptid1", Compressible, NotBinary, List("ptid"))
      lazy val `vnd.pwg-multiplexed`: MediaType                                                      =
        new MediaType("application", "vnd.pwg-multiplexed", Compressible, NotBinary)
      lazy val `vnd.pwg-xhtml-print+xml`: MediaType                                                  =
        new MediaType("application", "vnd.pwg-xhtml-print+xml", Compressible, NotBinary)
      lazy val `vnd.qualcomm.brew-app-res`: MediaType                                                =
        new MediaType("application", "vnd.qualcomm.brew-app-res", Compressible, NotBinary)
      lazy val `vnd.quarantainenet`: MediaType                                                       =
        new MediaType("application", "vnd.quarantainenet", Compressible, NotBinary)
      lazy val `vnd.quark.quarkxpress`: MediaType                                                    = new MediaType(
        "application",
        "vnd.quark.quarkxpress",
        Compressible,
        NotBinary,
        List("qxd", "qxt", "qwd", "qwt", "qxl", "qxb"),
      )
      lazy val `vnd.quobject-quoxdocument`: MediaType                                                =
        new MediaType("application", "vnd.quobject-quoxdocument", Compressible, NotBinary)
      lazy val `vnd.radisys.moml+xml`: MediaType                                                     =
        new MediaType("application", "vnd.radisys.moml+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml+xml`: MediaType                                                     =
        new MediaType("application", "vnd.radisys.msml+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml-audit+xml`: MediaType                                               =
        new MediaType("application", "vnd.radisys.msml-audit+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml-audit-conf+xml`: MediaType                                          =
        new MediaType("application", "vnd.radisys.msml-audit-conf+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml-audit-conn+xml`: MediaType                                          =
        new MediaType("application", "vnd.radisys.msml-audit-conn+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml-audit-dialog+xml`: MediaType                                        =
        new MediaType("application", "vnd.radisys.msml-audit-dialog+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml-audit-stream+xml`: MediaType                                        =
        new MediaType("application", "vnd.radisys.msml-audit-stream+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml-conf+xml`: MediaType                                                =
        new MediaType("application", "vnd.radisys.msml-conf+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml-dialog+xml`: MediaType                                              =
        new MediaType("application", "vnd.radisys.msml-dialog+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml-dialog-base+xml`: MediaType                                         =
        new MediaType("application", "vnd.radisys.msml-dialog-base+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml-dialog-fax-detect+xml`: MediaType                                   = new MediaType(
        "application",
        "vnd.radisys.msml-dialog-fax-detect+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.radisys.msml-dialog-fax-sendrecv+xml`: MediaType                                 = new MediaType(
        "application",
        "vnd.radisys.msml-dialog-fax-sendrecv+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.radisys.msml-dialog-group+xml`: MediaType                                        =
        new MediaType("application", "vnd.radisys.msml-dialog-group+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml-dialog-speech+xml`: MediaType                                       =
        new MediaType("application", "vnd.radisys.msml-dialog-speech+xml", Compressible, NotBinary)
      lazy val `vnd.radisys.msml-dialog-transform+xml`: MediaType                                    = new MediaType(
        "application",
        "vnd.radisys.msml-dialog-transform+xml",
        Compressible,
        NotBinary,
      )
      lazy val `vnd.rainstor.data`: MediaType                                                        =
        new MediaType("application", "vnd.rainstor.data", Compressible, NotBinary)
      lazy val `vnd.rapid`: MediaType                                                                =
        new MediaType("application", "vnd.rapid", Compressible, NotBinary)
      lazy val `vnd.rar`: MediaType                                                                  =
        new MediaType("application", "vnd.rar", Compressible, NotBinary, List("rar"))
      lazy val `vnd.realvnc.bed`: MediaType                                                          =
        new MediaType("application", "vnd.realvnc.bed", Compressible, NotBinary, List("bed"))
      lazy val `vnd.recordare.musicxml`: MediaType                                                   =
        new MediaType("application", "vnd.recordare.musicxml", Compressible, NotBinary, List("mxl"))
      lazy val `vnd.recordare.musicxml+xml`: MediaType                                               = new MediaType(
        "application",
        "vnd.recordare.musicxml+xml",
        Compressible,
        NotBinary,
        List("musicxml"),
      )
      lazy val `vnd.renlearn.rlprint`: MediaType                                                     =
        new MediaType("application", "vnd.renlearn.rlprint", Compressible, NotBinary)
      lazy val `vnd.resilient.logic`: MediaType                                                      =
        new MediaType("application", "vnd.resilient.logic", Compressible, NotBinary)
      lazy val `vnd.restful+json`: MediaType                                                         =
        new MediaType("application", "vnd.restful+json", Compressible, NotBinary)
      lazy val `vnd.rig.cryptonote`: MediaType                                                       = new MediaType(
        "application",
        "vnd.rig.cryptonote",
        Compressible,
        NotBinary,
        List("cryptonote"),
      )
      lazy val `vnd.rim.cod`: MediaType                                                              =
        new MediaType("application", "vnd.rim.cod", Compressible, NotBinary, List("cod"))
      lazy val `vnd.rn-realmedia`: MediaType                                                         =
        new MediaType("application", "vnd.rn-realmedia", Compressible, NotBinary, List("rm"))
      lazy val `vnd.rn-realmedia-vbr`: MediaType                                                     =
        new MediaType("application", "vnd.rn-realmedia-vbr", Compressible, NotBinary, List("rmvb"))
      lazy val `vnd.route66.link66+xml`: MediaType                                                   = new MediaType(
        "application",
        "vnd.route66.link66+xml",
        Compressible,
        NotBinary,
        List("link66"),
      )
      lazy val `vnd.rs-274x`: MediaType                                                              =
        new MediaType("application", "vnd.rs-274x", Compressible, NotBinary)
      lazy val `vnd.ruckus.download`: MediaType                                                      =
        new MediaType("application", "vnd.ruckus.download", Compressible, NotBinary)
      lazy val `vnd.s3sms`: MediaType                                                                =
        new MediaType("application", "vnd.s3sms", Compressible, NotBinary)
      lazy val `vnd.sailingtracker.track`: MediaType                                                 = new MediaType(
        "application",
        "vnd.sailingtracker.track",
        Compressible,
        NotBinary,
        List("st"),
      )
      lazy val `vnd.sar`: MediaType                                                                  =
        new MediaType("application", "vnd.sar", Compressible, NotBinary)
      lazy val `vnd.sbm.cid`: MediaType                                                              =
        new MediaType("application", "vnd.sbm.cid", Compressible, NotBinary)
      lazy val `vnd.sbm.mid2`: MediaType                                                             =
        new MediaType("application", "vnd.sbm.mid2", Compressible, NotBinary)
      lazy val `vnd.scribus`: MediaType                                                              =
        new MediaType("application", "vnd.scribus", Compressible, NotBinary)
      lazy val `vnd.sealed.3df`: MediaType                                                           =
        new MediaType("application", "vnd.sealed.3df", Compressible, NotBinary)
      lazy val `vnd.sealed.csf`: MediaType                                                           =
        new MediaType("application", "vnd.sealed.csf", Compressible, NotBinary)
      lazy val `vnd.sealed.doc`: MediaType                                                           =
        new MediaType("application", "vnd.sealed.doc", Compressible, NotBinary)
      lazy val `vnd.sealed.eml`: MediaType                                                           =
        new MediaType("application", "vnd.sealed.eml", Compressible, NotBinary)
      lazy val `vnd.sealed.mht`: MediaType                                                           =
        new MediaType("application", "vnd.sealed.mht", Compressible, NotBinary)
      lazy val `vnd.sealed.net`: MediaType                                                           =
        new MediaType("application", "vnd.sealed.net", Compressible, NotBinary)
      lazy val `vnd.sealed.ppt`: MediaType                                                           =
        new MediaType("application", "vnd.sealed.ppt", Compressible, NotBinary)
      lazy val `vnd.sealed.tiff`: MediaType                                                          =
        new MediaType("application", "vnd.sealed.tiff", Compressible, NotBinary)
      lazy val `vnd.sealed.xls`: MediaType                                                           =
        new MediaType("application", "vnd.sealed.xls", Compressible, NotBinary)
      lazy val `vnd.sealedmedia.softseal.html`: MediaType                                            =
        new MediaType("application", "vnd.sealedmedia.softseal.html", Compressible, NotBinary)
      lazy val `vnd.sealedmedia.softseal.pdf`: MediaType                                             =
        new MediaType("application", "vnd.sealedmedia.softseal.pdf", Compressible, NotBinary)
      lazy val `vnd.seemail`: MediaType                                                              =
        new MediaType("application", "vnd.seemail", Compressible, NotBinary, List("see"))
      lazy val `vnd.seis+json`: MediaType                                                            =
        new MediaType("application", "vnd.seis+json", Compressible, NotBinary)
      lazy val `vnd.sema`: MediaType                                                                 =
        new MediaType("application", "vnd.sema", Compressible, NotBinary, List("sema"))
      lazy val `vnd.semd`: MediaType                                                                 =
        new MediaType("application", "vnd.semd", Compressible, NotBinary, List("semd"))
      lazy val `vnd.semf`: MediaType                                                                 =
        new MediaType("application", "vnd.semf", Compressible, NotBinary, List("semf"))
      lazy val `vnd.shade-save-file`: MediaType                                                      =
        new MediaType("application", "vnd.shade-save-file", Compressible, NotBinary)
      lazy val `vnd.shana.informed.formdata`: MediaType                                              = new MediaType(
        "application",
        "vnd.shana.informed.formdata",
        Compressible,
        NotBinary,
        List("ifm"),
      )
      lazy val `vnd.shana.informed.formtemplate`: MediaType                                          = new MediaType(
        "application",
        "vnd.shana.informed.formtemplate",
        Compressible,
        NotBinary,
        List("itp"),
      )
      lazy val `vnd.shana.informed.interchange`: MediaType                                           = new MediaType(
        "application",
        "vnd.shana.informed.interchange",
        Compressible,
        NotBinary,
        List("iif"),
      )
      lazy val `vnd.shana.informed.package`: MediaType                                               = new MediaType(
        "application",
        "vnd.shana.informed.package",
        Compressible,
        NotBinary,
        List("ipk"),
      )
      lazy val `vnd.shootproof+json`: MediaType                                                      =
        new MediaType("application", "vnd.shootproof+json", Compressible, NotBinary)
      lazy val `vnd.shopkick+json`: MediaType                                                        =
        new MediaType("application", "vnd.shopkick+json", Compressible, NotBinary)
      lazy val `vnd.shp`: MediaType                                                                  =
        new MediaType("application", "vnd.shp", Compressible, NotBinary)
      lazy val `vnd.shx`: MediaType                                                                  =
        new MediaType("application", "vnd.shx", Compressible, NotBinary)
      lazy val `vnd.sigrok.session`: MediaType                                                       =
        new MediaType("application", "vnd.sigrok.session", Compressible, NotBinary)
      lazy val `vnd.simtech-mindmapper`: MediaType                                                   = new MediaType(
        "application",
        "vnd.simtech-mindmapper",
        Compressible,
        NotBinary,
        List("twd", "twds"),
      )
      lazy val `vnd.siren+json`: MediaType                                                           =
        new MediaType("application", "vnd.siren+json", Compressible, NotBinary)
      lazy val `vnd.smaf`: MediaType                                                                 =
        new MediaType("application", "vnd.smaf", Compressible, NotBinary, List("mmf"))
      lazy val `vnd.smart.notebook`: MediaType                                                       =
        new MediaType("application", "vnd.smart.notebook", Compressible, NotBinary)
      lazy val `vnd.smart.teacher`: MediaType                                                        =
        new MediaType("application", "vnd.smart.teacher", Compressible, NotBinary, List("teacher"))
      lazy val `vnd.snesdev-page-table`: MediaType                                                   =
        new MediaType("application", "vnd.snesdev-page-table", Compressible, NotBinary)
      lazy val `vnd.software602.filler.form+xml`: MediaType                                          = new MediaType(
        "application",
        "vnd.software602.filler.form+xml",
        Compressible,
        NotBinary,
        List("fo"),
      )
      lazy val `vnd.software602.filler.form-xml-zip`: MediaType                                      =
        new MediaType("application", "vnd.software602.filler.form-xml-zip", Compressible, NotBinary)
      lazy val `vnd.solent.sdkm+xml`: MediaType                                                      = new MediaType(
        "application",
        "vnd.solent.sdkm+xml",
        Compressible,
        NotBinary,
        List("sdkm", "sdkd"),
      )
      lazy val `vnd.spotfire.dxp`: MediaType                                                         =
        new MediaType("application", "vnd.spotfire.dxp", Compressible, NotBinary, List("dxp"))
      lazy val `vnd.spotfire.sfs`: MediaType                                                         =
        new MediaType("application", "vnd.spotfire.sfs", Compressible, NotBinary, List("sfs"))
      lazy val `vnd.sqlite3`: MediaType                                                              =
        new MediaType("application", "vnd.sqlite3", Compressible, NotBinary)
      lazy val `vnd.sss-cod`: MediaType                                                              =
        new MediaType("application", "vnd.sss-cod", Compressible, NotBinary)
      lazy val `vnd.sss-dtf`: MediaType                                                              =
        new MediaType("application", "vnd.sss-dtf", Compressible, NotBinary)
      lazy val `vnd.sss-ntf`: MediaType                                                              =
        new MediaType("application", "vnd.sss-ntf", Compressible, NotBinary)
      lazy val `vnd.stardivision.calc`: MediaType                                                    =
        new MediaType("application", "vnd.stardivision.calc", Compressible, NotBinary, List("sdc"))
      lazy val `vnd.stardivision.draw`: MediaType                                                    =
        new MediaType("application", "vnd.stardivision.draw", Compressible, NotBinary, List("sda"))
      lazy val `vnd.stardivision.impress`: MediaType                                                 = new MediaType(
        "application",
        "vnd.stardivision.impress",
        Compressible,
        NotBinary,
        List("sdd"),
      )
      lazy val `vnd.stardivision.math`: MediaType                                                    =
        new MediaType("application", "vnd.stardivision.math", Compressible, NotBinary, List("smf"))
      lazy val `vnd.stardivision.writer`: MediaType                                                  = new MediaType(
        "application",
        "vnd.stardivision.writer",
        Compressible,
        NotBinary,
        List("sdw", "vor"),
      )
      lazy val `vnd.stardivision.writer-global`: MediaType                                           = new MediaType(
        "application",
        "vnd.stardivision.writer-global",
        Compressible,
        NotBinary,
        List("sgl"),
      )
      lazy val `vnd.stepmania.package`: MediaType                                                    = new MediaType(
        "application",
        "vnd.stepmania.package",
        Compressible,
        NotBinary,
        List("smzip"),
      )
      lazy val `vnd.stepmania.stepchart`: MediaType                                                  =
        new MediaType("application", "vnd.stepmania.stepchart", Compressible, NotBinary, List("sm"))
      lazy val `vnd.street-stream`: MediaType                                                        =
        new MediaType("application", "vnd.street-stream", Compressible, NotBinary)
      lazy val `vnd.sun.wadl+xml`: MediaType                                                         =
        new MediaType("application", "vnd.sun.wadl+xml", Compressible, NotBinary, List("wadl"))
      lazy val `vnd.sun.xml.calc`: MediaType                                                         =
        new MediaType("application", "vnd.sun.xml.calc", Compressible, NotBinary, List("sxc"))
      lazy val `vnd.sun.xml.calc.template`: MediaType                                                = new MediaType(
        "application",
        "vnd.sun.xml.calc.template",
        Compressible,
        NotBinary,
        List("stc"),
      )
      lazy val `vnd.sun.xml.draw`: MediaType                                                         =
        new MediaType("application", "vnd.sun.xml.draw", Compressible, NotBinary, List("sxd"))
      lazy val `vnd.sun.xml.draw.template`: MediaType                                                = new MediaType(
        "application",
        "vnd.sun.xml.draw.template",
        Compressible,
        NotBinary,
        List("std"),
      )
      lazy val `vnd.sun.xml.impress`: MediaType                                                      =
        new MediaType("application", "vnd.sun.xml.impress", Compressible, NotBinary, List("sxi"))
      lazy val `vnd.sun.xml.impress.template`: MediaType                                             = new MediaType(
        "application",
        "vnd.sun.xml.impress.template",
        Compressible,
        NotBinary,
        List("sti"),
      )
      lazy val `vnd.sun.xml.math`: MediaType                                                         =
        new MediaType("application", "vnd.sun.xml.math", Compressible, NotBinary, List("sxm"))
      lazy val `vnd.sun.xml.writer`: MediaType                                                       =
        new MediaType("application", "vnd.sun.xml.writer", Compressible, NotBinary, List("sxw"))
      lazy val `vnd.sun.xml.writer.global`: MediaType                                                = new MediaType(
        "application",
        "vnd.sun.xml.writer.global",
        Compressible,
        NotBinary,
        List("sxg"),
      )
      lazy val `vnd.sun.xml.writer.template`: MediaType                                              = new MediaType(
        "application",
        "vnd.sun.xml.writer.template",
        Compressible,
        NotBinary,
        List("stw"),
      )
      lazy val `vnd.sus-calendar`: MediaType                                                         = new MediaType(
        "application",
        "vnd.sus-calendar",
        Compressible,
        NotBinary,
        List("sus", "susp"),
      )
      lazy val `vnd.svd`: MediaType                                                                  =
        new MediaType("application", "vnd.svd", Compressible, NotBinary, List("svd"))
      lazy val `vnd.swiftview-ics`: MediaType                                                        =
        new MediaType("application", "vnd.swiftview-ics", Compressible, NotBinary)
      lazy val `vnd.sycle+xml`: MediaType                                                            =
        new MediaType("application", "vnd.sycle+xml", Compressible, NotBinary)
      lazy val `vnd.symbian.install`: MediaType                                                      = new MediaType(
        "application",
        "vnd.symbian.install",
        Compressible,
        NotBinary,
        List("sis", "sisx"),
      )
      lazy val `vnd.syncml+xml`: MediaType                                                           =
        new MediaType("application", "vnd.syncml+xml", Compressible, NotBinary, List("xsm"))
      lazy val `vnd.syncml.dm+wbxml`: MediaType                                                      =
        new MediaType("application", "vnd.syncml.dm+wbxml", Compressible, NotBinary, List("bdm"))
      lazy val `vnd.syncml.dm+xml`: MediaType                                                        =
        new MediaType("application", "vnd.syncml.dm+xml", Compressible, NotBinary, List("xdm"))
      lazy val `vnd.syncml.dm.notification`: MediaType                                               =
        new MediaType("application", "vnd.syncml.dm.notification", Compressible, NotBinary)
      lazy val `vnd.syncml.dmddf+wbxml`: MediaType                                                   =
        new MediaType("application", "vnd.syncml.dmddf+wbxml", Compressible, NotBinary)
      lazy val `vnd.syncml.dmddf+xml`: MediaType                                                     =
        new MediaType("application", "vnd.syncml.dmddf+xml", Compressible, NotBinary, List("ddf"))
      lazy val `vnd.syncml.dmtnds+wbxml`: MediaType                                                  =
        new MediaType("application", "vnd.syncml.dmtnds+wbxml", Compressible, NotBinary)
      lazy val `vnd.syncml.dmtnds+xml`: MediaType                                                    =
        new MediaType("application", "vnd.syncml.dmtnds+xml", Compressible, NotBinary)
      lazy val `vnd.syncml.ds.notification`: MediaType                                               =
        new MediaType("application", "vnd.syncml.ds.notification", Compressible, NotBinary)
      lazy val `vnd.tableschema+json`: MediaType                                                     =
        new MediaType("application", "vnd.tableschema+json", Compressible, NotBinary)
      lazy val `vnd.tao.intent-module-archive`: MediaType                                            = new MediaType(
        "application",
        "vnd.tao.intent-module-archive",
        Compressible,
        NotBinary,
        List("tao"),
      )
      lazy val `vnd.tcpdump.pcap`: MediaType                                                         = new MediaType(
        "application",
        "vnd.tcpdump.pcap",
        Compressible,
        NotBinary,
        List("pcap", "cap", "dmp"),
      )
      lazy val `vnd.think-cell.ppttc+json`: MediaType                                                =
        new MediaType("application", "vnd.think-cell.ppttc+json", Compressible, NotBinary)
      lazy val `vnd.tmd.mediaflex.api+xml`: MediaType                                                =
        new MediaType("application", "vnd.tmd.mediaflex.api+xml", Compressible, NotBinary)
      lazy val `vnd.tml`: MediaType                                                                  =
        new MediaType("application", "vnd.tml", Compressible, NotBinary)
      lazy val `vnd.tmobile-livetv`: MediaType                                                       =
        new MediaType("application", "vnd.tmobile-livetv", Compressible, NotBinary, List("tmo"))
      lazy val `vnd.tri.onesource`: MediaType                                                        =
        new MediaType("application", "vnd.tri.onesource", Compressible, NotBinary)
      lazy val `vnd.trid.tpt`: MediaType                                                             =
        new MediaType("application", "vnd.trid.tpt", Compressible, NotBinary, List("tpt"))
      lazy val `vnd.triscape.mxs`: MediaType                                                         =
        new MediaType("application", "vnd.triscape.mxs", Compressible, NotBinary, List("mxs"))
      lazy val `vnd.trueapp`: MediaType                                                              =
        new MediaType("application", "vnd.trueapp", Compressible, NotBinary, List("tra"))
      lazy val `vnd.truedoc`: MediaType                                                              =
        new MediaType("application", "vnd.truedoc", Compressible, NotBinary)
      lazy val `vnd.ubisoft.webplayer`: MediaType                                                    =
        new MediaType("application", "vnd.ubisoft.webplayer", Compressible, NotBinary)
      lazy val `vnd.ufdl`: MediaType                                                                 =
        new MediaType("application", "vnd.ufdl", Compressible, NotBinary, List("ufd", "ufdl"))
      lazy val `vnd.uiq.theme`: MediaType                                                            =
        new MediaType("application", "vnd.uiq.theme", Compressible, NotBinary, List("utz"))
      lazy val `vnd.umajin`: MediaType                                                               =
        new MediaType("application", "vnd.umajin", Compressible, NotBinary, List("umj"))
      lazy val `vnd.unity`: MediaType                                                                =
        new MediaType("application", "vnd.unity", Compressible, NotBinary, List("unityweb"))
      lazy val `vnd.uoml+xml`: MediaType                                                             =
        new MediaType("application", "vnd.uoml+xml", Compressible, NotBinary, List("uoml"))
      lazy val `vnd.uplanet.alert`: MediaType                                                        =
        new MediaType("application", "vnd.uplanet.alert", Compressible, NotBinary)
      lazy val `vnd.uplanet.alert-wbxml`: MediaType                                                  =
        new MediaType("application", "vnd.uplanet.alert-wbxml", Compressible, NotBinary)
      lazy val `vnd.uplanet.bearer-choice`: MediaType                                                =
        new MediaType("application", "vnd.uplanet.bearer-choice", Compressible, NotBinary)
      lazy val `vnd.uplanet.bearer-choice-wbxml`: MediaType                                          =
        new MediaType("application", "vnd.uplanet.bearer-choice-wbxml", Compressible, NotBinary)
      lazy val `vnd.uplanet.cacheop`: MediaType                                                      =
        new MediaType("application", "vnd.uplanet.cacheop", Compressible, NotBinary)
      lazy val `vnd.uplanet.cacheop-wbxml`: MediaType                                                =
        new MediaType("application", "vnd.uplanet.cacheop-wbxml", Compressible, NotBinary)
      lazy val `vnd.uplanet.channel`: MediaType                                                      =
        new MediaType("application", "vnd.uplanet.channel", Compressible, NotBinary)
      lazy val `vnd.uplanet.channel-wbxml`: MediaType                                                =
        new MediaType("application", "vnd.uplanet.channel-wbxml", Compressible, NotBinary)
      lazy val `vnd.uplanet.list`: MediaType                                                         =
        new MediaType("application", "vnd.uplanet.list", Compressible, NotBinary)
      lazy val `vnd.uplanet.list-wbxml`: MediaType                                                   =
        new MediaType("application", "vnd.uplanet.list-wbxml", Compressible, NotBinary)
      lazy val `vnd.uplanet.listcmd`: MediaType                                                      =
        new MediaType("application", "vnd.uplanet.listcmd", Compressible, NotBinary)
      lazy val `vnd.uplanet.listcmd-wbxml`: MediaType                                                =
        new MediaType("application", "vnd.uplanet.listcmd-wbxml", Compressible, NotBinary)
      lazy val `vnd.uplanet.signal`: MediaType                                                       =
        new MediaType("application", "vnd.uplanet.signal", Compressible, NotBinary)
      lazy val `vnd.uri-map`: MediaType                                                              =
        new MediaType("application", "vnd.uri-map", Compressible, NotBinary)
      lazy val `vnd.valve.source.material`: MediaType                                                =
        new MediaType("application", "vnd.valve.source.material", Compressible, NotBinary)
      lazy val `vnd.vcx`: MediaType                                                                  =
        new MediaType("application", "vnd.vcx", Compressible, NotBinary, List("vcx"))
      lazy val `vnd.vd-study`: MediaType                                                             =
        new MediaType("application", "vnd.vd-study", Compressible, NotBinary)
      lazy val `vnd.vectorworks`: MediaType                                                          =
        new MediaType("application", "vnd.vectorworks", Compressible, NotBinary)
      lazy val `vnd.vel+json`: MediaType                                                             =
        new MediaType("application", "vnd.vel+json", Compressible, NotBinary)
      lazy val `vnd.verimatrix.vcas`: MediaType                                                      =
        new MediaType("application", "vnd.verimatrix.vcas", Compressible, NotBinary)
      lazy val `vnd.veryant.thin`: MediaType                                                         =
        new MediaType("application", "vnd.veryant.thin", Compressible, NotBinary)
      lazy val `vnd.ves.encrypted`: MediaType                                                        =
        new MediaType("application", "vnd.ves.encrypted", Compressible, NotBinary)
      lazy val `vnd.vidsoft.vidconference`: MediaType                                                =
        new MediaType("application", "vnd.vidsoft.vidconference", Compressible, NotBinary)
      lazy val `vnd.visio`: MediaType                                                                = new MediaType(
        "application",
        "vnd.visio",
        Compressible,
        NotBinary,
        List("vsd", "vst", "vss", "vsw"),
      )
      lazy val `vnd.visionary`: MediaType                                                            =
        new MediaType("application", "vnd.visionary", Compressible, NotBinary, List("vis"))
      lazy val `vnd.vividence.scriptfile`: MediaType                                                 =
        new MediaType("application", "vnd.vividence.scriptfile", Compressible, NotBinary)
      lazy val `vnd.vsf`: MediaType                                                                  =
        new MediaType("application", "vnd.vsf", Compressible, NotBinary, List("vsf"))
      lazy val `vnd.wap.sic`: MediaType                                                              =
        new MediaType("application", "vnd.wap.sic", Compressible, NotBinary)
      lazy val `vnd.wap.slc`: MediaType                                                              =
        new MediaType("application", "vnd.wap.slc", Compressible, NotBinary)
      lazy val `vnd.wap.wbxml`: MediaType                                                            =
        new MediaType("application", "vnd.wap.wbxml", Compressible, NotBinary, List("wbxml"))
      lazy val `vnd.wap.wmlc`: MediaType                                                             =
        new MediaType("application", "vnd.wap.wmlc", Compressible, NotBinary, List("wmlc"))
      lazy val `vnd.wap.wmlscriptc`: MediaType                                                       =
        new MediaType("application", "vnd.wap.wmlscriptc", Compressible, NotBinary, List("wmlsc"))
      lazy val `vnd.webturbo`: MediaType                                                             =
        new MediaType("application", "vnd.webturbo", Compressible, NotBinary, List("wtb"))
      lazy val `vnd.wfa.dpp`: MediaType                                                              =
        new MediaType("application", "vnd.wfa.dpp", Compressible, NotBinary)
      lazy val `vnd.wfa.p2p`: MediaType                                                              =
        new MediaType("application", "vnd.wfa.p2p", Compressible, NotBinary)
      lazy val `vnd.wfa.wsc`: MediaType                                                              =
        new MediaType("application", "vnd.wfa.wsc", Compressible, NotBinary)
      lazy val `vnd.windows.devicepairing`: MediaType                                                =
        new MediaType("application", "vnd.windows.devicepairing", Compressible, NotBinary)
      lazy val `vnd.wmc`: MediaType                                                                  =
        new MediaType("application", "vnd.wmc", Compressible, NotBinary)
      lazy val `vnd.wmf.bootstrap`: MediaType                                                        =
        new MediaType("application", "vnd.wmf.bootstrap", Compressible, NotBinary)
      lazy val `vnd.wolfram.mathematica`: MediaType                                                  =
        new MediaType("application", "vnd.wolfram.mathematica", Compressible, NotBinary)
      lazy val `vnd.wolfram.mathematica.package`: MediaType                                          =
        new MediaType("application", "vnd.wolfram.mathematica.package", Compressible, NotBinary)
      lazy val `vnd.wolfram.player`: MediaType                                                       =
        new MediaType("application", "vnd.wolfram.player", Compressible, NotBinary, List("nbp"))
      lazy val `vnd.wordperfect`: MediaType                                                          =
        new MediaType("application", "vnd.wordperfect", Compressible, NotBinary, List("wpd"))
      lazy val `vnd.wqd`: MediaType                                                                  =
        new MediaType("application", "vnd.wqd", Compressible, NotBinary, List("wqd"))
      lazy val `vnd.wrq-hp3000-labelled`: MediaType                                                  =
        new MediaType("application", "vnd.wrq-hp3000-labelled", Compressible, NotBinary)
      lazy val `vnd.wt.stf`: MediaType                                                               =
        new MediaType("application", "vnd.wt.stf", Compressible, NotBinary, List("stf"))
      lazy val `vnd.wv.csp+wbxml`: MediaType                                                         =
        new MediaType("application", "vnd.wv.csp+wbxml", Compressible, NotBinary)
      lazy val `vnd.wv.csp+xml`: MediaType                                                           =
        new MediaType("application", "vnd.wv.csp+xml", Compressible, NotBinary)
      lazy val `vnd.wv.ssp+xml`: MediaType                                                           =
        new MediaType("application", "vnd.wv.ssp+xml", Compressible, NotBinary)
      lazy val `vnd.xacml+json`: MediaType                                                           =
        new MediaType("application", "vnd.xacml+json", Compressible, NotBinary)
      lazy val `vnd.xara`: MediaType                                                                 =
        new MediaType("application", "vnd.xara", Compressible, NotBinary, List("xar"))
      lazy val `vnd.xfdl`: MediaType                                                                 =
        new MediaType("application", "vnd.xfdl", Compressible, NotBinary, List("xfdl"))
      lazy val `vnd.xfdl.webform`: MediaType                                                         =
        new MediaType("application", "vnd.xfdl.webform", Compressible, NotBinary)
      lazy val `vnd.xmi+xml`: MediaType                                                              =
        new MediaType("application", "vnd.xmi+xml", Compressible, NotBinary)
      lazy val `vnd.xmpie.cpkg`: MediaType                                                           =
        new MediaType("application", "vnd.xmpie.cpkg", Compressible, NotBinary)
      lazy val `vnd.xmpie.dpkg`: MediaType                                                           =
        new MediaType("application", "vnd.xmpie.dpkg", Compressible, NotBinary)
      lazy val `vnd.xmpie.plan`: MediaType                                                           =
        new MediaType("application", "vnd.xmpie.plan", Compressible, NotBinary)
      lazy val `vnd.xmpie.ppkg`: MediaType                                                           =
        new MediaType("application", "vnd.xmpie.ppkg", Compressible, NotBinary)
      lazy val `vnd.xmpie.xlim`: MediaType                                                           =
        new MediaType("application", "vnd.xmpie.xlim", Compressible, NotBinary)
      lazy val `vnd.yamaha.hv-dic`: MediaType                                                        =
        new MediaType("application", "vnd.yamaha.hv-dic", Compressible, NotBinary, List("hvd"))
      lazy val `vnd.yamaha.hv-script`: MediaType                                                     =
        new MediaType("application", "vnd.yamaha.hv-script", Compressible, NotBinary, List("hvs"))
      lazy val `vnd.yamaha.hv-voice`: MediaType                                                      =
        new MediaType("application", "vnd.yamaha.hv-voice", Compressible, NotBinary, List("hvp"))
      lazy val `vnd.yamaha.openscoreformat`: MediaType                                               = new MediaType(
        "application",
        "vnd.yamaha.openscoreformat",
        Compressible,
        NotBinary,
        List("osf"),
      )
      lazy val `vnd.yamaha.openscoreformat.osfpvg+xml`: MediaType                                    = new MediaType(
        "application",
        "vnd.yamaha.openscoreformat.osfpvg+xml",
        Compressible,
        NotBinary,
        List("osfpvg"),
      )
      lazy val `vnd.yamaha.remote-setup`: MediaType                                                  =
        new MediaType("application", "vnd.yamaha.remote-setup", Compressible, NotBinary)
      lazy val `vnd.yamaha.smaf-audio`: MediaType                                                    =
        new MediaType("application", "vnd.yamaha.smaf-audio", Compressible, NotBinary, List("saf"))
      lazy val `vnd.yamaha.smaf-phrase`: MediaType                                                   =
        new MediaType("application", "vnd.yamaha.smaf-phrase", Compressible, NotBinary, List("spf"))
      lazy val `vnd.yamaha.through-ngn`: MediaType                                                   =
        new MediaType("application", "vnd.yamaha.through-ngn", Compressible, NotBinary)
      lazy val `vnd.yamaha.tunnel-udpencap`: MediaType                                               =
        new MediaType("application", "vnd.yamaha.tunnel-udpencap", Compressible, NotBinary)
      lazy val `vnd.yaoweme`: MediaType                                                              =
        new MediaType("application", "vnd.yaoweme", Compressible, NotBinary)
      lazy val `vnd.yellowriver-custom-menu`: MediaType                                              = new MediaType(
        "application",
        "vnd.yellowriver-custom-menu",
        Compressible,
        NotBinary,
        List("cmp"),
      )
      lazy val `vnd.youtube.yt`: MediaType                                                           =
        new MediaType("application", "vnd.youtube.yt", Compressible, NotBinary)
      lazy val `vnd.zul`: MediaType                                                                  =
        new MediaType("application", "vnd.zul", Compressible, NotBinary, List("zir", "zirz"))
      lazy val `vnd.zzazz.deck+xml`: MediaType                                                       =
        new MediaType("application", "vnd.zzazz.deck+xml", Compressible, NotBinary, List("zaz"))
      lazy val `voicexml+xml`: MediaType                                                             =
        new MediaType("application", "voicexml+xml", Compressible, NotBinary, List("vxml"))
      lazy val `voucher-cms+json`: MediaType                                                         =
        new MediaType("application", "voucher-cms+json", Compressible, NotBinary)
      lazy val `vq-rtcpxr`: MediaType                                                                =
        new MediaType("application", "vq-rtcpxr", Compressible, NotBinary)
      lazy val `wasm`: MediaType                                                                     =
        new MediaType("application", "wasm", Compressible, NotBinary, List("wasm"))
      lazy val `watcherinfo+xml`: MediaType                                                          =
        new MediaType("application", "watcherinfo+xml", Compressible, NotBinary)
      lazy val `webpush-options+json`: MediaType                                                     =
        new MediaType("application", "webpush-options+json", Compressible, NotBinary)
      lazy val `whoispp-query`: MediaType                                                            =
        new MediaType("application", "whoispp-query", Compressible, NotBinary)
      lazy val `whoispp-response`: MediaType                                                         =
        new MediaType("application", "whoispp-response", Compressible, NotBinary)
      lazy val `widget`: MediaType                                                                   =
        new MediaType("application", "widget", Compressible, NotBinary, List("wgt"))
      lazy val `winhlp`: MediaType                                                                   =
        new MediaType("application", "winhlp", Compressible, NotBinary, List("hlp"))
      lazy val `wita`: MediaType               = new MediaType("application", "wita", Compressible, NotBinary)
      lazy val `wordperfect5.1`: MediaType     =
        new MediaType("application", "wordperfect5.1", Compressible, NotBinary)
      lazy val `wsdl+xml`: MediaType           =
        new MediaType("application", "wsdl+xml", Compressible, NotBinary, List("wsdl"))
      lazy val `wspolicy+xml`: MediaType       =
        new MediaType("application", "wspolicy+xml", Compressible, NotBinary, List("wspolicy"))
      lazy val `x-7z-compressed`: MediaType    =
        new MediaType("application", "x-7z-compressed", Uncompressible, Binary, List("7z"))
      lazy val `x-abiword`: MediaType          =
        new MediaType("application", "x-abiword", Compressible, NotBinary, List("abw"))
      lazy val `x-ace-compressed`: MediaType   =
        new MediaType("application", "x-ace-compressed", Compressible, Binary, List("ace"))
      lazy val `x-amf`: MediaType              = new MediaType("application", "x-amf", Compressible, NotBinary)
      lazy val `x-apple-diskimage`: MediaType  =
        new MediaType("application", "x-apple-diskimage", Compressible, Binary, List("dmg"))
      lazy val `x-arj`: MediaType              =
        new MediaType("application", "x-arj", Uncompressible, NotBinary, List("arj"))
      lazy val `x-authorware-bin`: MediaType   = new MediaType(
        "application",
        "x-authorware-bin",
        Compressible,
        NotBinary,
        List("aab", "x32", "u32", "vox"),
      )
      lazy val `x-authorware-map`: MediaType   =
        new MediaType("application", "x-authorware-map", Compressible, NotBinary, List("aam"))
      lazy val `x-authorware-seg`: MediaType   =
        new MediaType("application", "x-authorware-seg", Compressible, NotBinary, List("aas"))
      lazy val `x-bcpio`: MediaType            =
        new MediaType("application", "x-bcpio", Compressible, NotBinary, List("bcpio"))
      lazy val `x-bdoc`: MediaType             =
        new MediaType("application", "x-bdoc", Uncompressible, NotBinary, List("bdoc"))
      lazy val `x-bittorrent`: MediaType       =
        new MediaType("application", "x-bittorrent", Compressible, NotBinary, List("torrent"))
      lazy val `x-blorb`: MediaType            =
        new MediaType("application", "x-blorb", Compressible, NotBinary, List("blb", "blorb"))
      lazy val `x-bzip`: MediaType             =
        new MediaType("application", "x-bzip", Uncompressible, Binary, List("bz"))
      lazy val `x-bzip2`: MediaType            =
        new MediaType("application", "x-bzip2", Uncompressible, Binary, List("bz2", "boz"))
      lazy val `x-cbr`: MediaType              = new MediaType(
        "application",
        "x-cbr",
        Compressible,
        NotBinary,
        List("cbr", "cba", "cbt", "cbz", "cb7"),
      )
      lazy val `x-cdlink`: MediaType           =
        new MediaType("application", "x-cdlink", Compressible, NotBinary, List("vcd"))
      lazy val `x-cfs-compressed`: MediaType   =
        new MediaType("application", "x-cfs-compressed", Compressible, NotBinary, List("cfs"))
      lazy val `x-chat`: MediaType             =
        new MediaType("application", "x-chat", Compressible, NotBinary, List("chat"))
      lazy val `x-chess-pgn`: MediaType        =
        new MediaType("application", "x-chess-pgn", Compressible, NotBinary, List("pgn"))
      lazy val `x-chrome-extension`: MediaType =
        new MediaType("application", "x-chrome-extension", Compressible, Binary, List("crx"))
      lazy val `x-cocoa`: MediaType            =
        new MediaType("application", "x-cocoa", Compressible, NotBinary, List("cco"))
      lazy val `x-compress`: MediaType         =
        new MediaType("application", "x-compress", Compressible, Binary)
      lazy val `x-conference`: MediaType       =
        new MediaType("application", "x-conference", Compressible, NotBinary, List("nsc"))
      lazy val `x-cpio`: MediaType             =
        new MediaType("application", "x-cpio", Compressible, NotBinary, List("cpio"))
      lazy val part_2: List[MediaType]         = List(
        `vnd.ms-wmdrm.lic-resp`,
        `vnd.ms-wmdrm.meter-chlg-req`,
        `vnd.ms-wmdrm.meter-resp`,
        `vnd.ms-word.document.macroenabled.12`,
        `vnd.ms-word.template.macroenabled.12`,
        `vnd.ms-works`,
        `vnd.ms-wpl`,
        `vnd.ms-xpsdocument`,
        `vnd.msa-disk-image`,
        `vnd.mseq`,
        `vnd.msign`,
        `vnd.multiad.creator`,
        `vnd.multiad.creator.cif`,
        `vnd.music-niff`,
        `vnd.musician`,
        `vnd.muvee.style`,
        `vnd.mynfc`,
        `vnd.ncd.control`,
        `vnd.ncd.reference`,
        `vnd.nearst.inv+json`,
        `vnd.nebumind.line`,
        `vnd.nervana`,
        `vnd.netfpx`,
        `vnd.neurolanguage.nlu`,
        `vnd.nimn`,
        `vnd.nintendo.nitro.rom`,
        `vnd.nintendo.snes.rom`,
        `vnd.nitf`,
        `vnd.noblenet-directory`,
        `vnd.noblenet-sealer`,
        `vnd.noblenet-web`,
        `vnd.nokia.catalogs`,
        `vnd.nokia.conml+wbxml`,
        `vnd.nokia.conml+xml`,
        `vnd.nokia.iptv.config+xml`,
        `vnd.nokia.isds-radio-presets`,
        `vnd.nokia.landmark+wbxml`,
        `vnd.nokia.landmark+xml`,
        `vnd.nokia.landmarkcollection+xml`,
        `vnd.nokia.n-gage.ac+xml`,
        `vnd.nokia.n-gage.data`,
        `vnd.nokia.n-gage.symbian.install`,
        `vnd.nokia.ncd`,
        `vnd.nokia.pcd+wbxml`,
        `vnd.nokia.pcd+xml`,
        `vnd.nokia.radio-preset`,
        `vnd.nokia.radio-presets`,
        `vnd.novadigm.edm`,
        `vnd.novadigm.edx`,
        `vnd.novadigm.ext`,
        `vnd.ntt-local.content-share`,
        `vnd.ntt-local.file-transfer`,
        `vnd.ntt-local.ogw_remote-access`,
        `vnd.ntt-local.sip-ta_remote`,
        `vnd.ntt-local.sip-ta_tcp_stream`,
        `vnd.oasis.opendocument.chart`,
        `vnd.oasis.opendocument.chart-template`,
        `vnd.oasis.opendocument.database`,
        `vnd.oasis.opendocument.formula`,
        `vnd.oasis.opendocument.formula-template`,
        `vnd.oasis.opendocument.graphics`,
        `vnd.oasis.opendocument.graphics-template`,
        `vnd.oasis.opendocument.image`,
        `vnd.oasis.opendocument.image-template`,
        `vnd.oasis.opendocument.presentation`,
        `vnd.oasis.opendocument.presentation-template`,
        `vnd.oasis.opendocument.spreadsheet`,
        `vnd.oasis.opendocument.spreadsheet-template`,
        `vnd.oasis.opendocument.text`,
        `vnd.oasis.opendocument.text-master`,
        `vnd.oasis.opendocument.text-template`,
        `vnd.oasis.opendocument.text-web`,
        `vnd.obn`,
        `vnd.ocf+cbor`,
        `vnd.oci.image.manifest.v1+json`,
        `vnd.oftn.l10n+json`,
        `vnd.oipf.contentaccessdownload+xml`,
        `vnd.oipf.contentaccessstreaming+xml`,
        `vnd.oipf.cspg-hexbinary`,
        `vnd.oipf.dae.svg+xml`,
        `vnd.oipf.dae.xhtml+xml`,
        `vnd.oipf.mippvcontrolmessage+xml`,
        `vnd.oipf.pae.gem`,
        `vnd.oipf.spdiscovery+xml`,
        `vnd.oipf.spdlist+xml`,
        `vnd.oipf.ueprofile+xml`,
        `vnd.oipf.userprofile+xml`,
        `vnd.olpc-sugar`,
        `vnd.oma-scws-config`,
        `vnd.oma-scws-http-request`,
        `vnd.oma-scws-http-response`,
        `vnd.oma.bcast.associated-procedure-parameter+xml`,
        `vnd.oma.bcast.drm-trigger+xml`,
        `vnd.oma.bcast.imd+xml`,
        `vnd.oma.bcast.ltkm`,
        `vnd.oma.bcast.notification+xml`,
        `vnd.oma.bcast.provisioningtrigger`,
        `vnd.oma.bcast.sgboot`,
        `vnd.oma.bcast.sgdd+xml`,
        `vnd.oma.bcast.sgdu`,
        `vnd.oma.bcast.simple-symbol-container`,
        `vnd.oma.bcast.smartcard-trigger+xml`,
        `vnd.oma.bcast.sprov+xml`,
        `vnd.oma.bcast.stkm`,
        `vnd.oma.cab-address-book+xml`,
        `vnd.oma.cab-feature-handler+xml`,
        `vnd.oma.cab-pcc+xml`,
        `vnd.oma.cab-subs-invite+xml`,
        `vnd.oma.cab-user-prefs+xml`,
        `vnd.oma.dcd`,
        `vnd.oma.dcdc`,
        `vnd.oma.dd2+xml`,
        `vnd.oma.drm.risd+xml`,
        `vnd.oma.group-usage-list+xml`,
        `vnd.oma.lwm2m+cbor`,
        `vnd.oma.lwm2m+json`,
        `vnd.oma.lwm2m+tlv`,
        `vnd.oma.pal+xml`,
        `vnd.oma.poc.detailed-progress-report+xml`,
        `vnd.oma.poc.final-report+xml`,
        `vnd.oma.poc.groups+xml`,
        `vnd.oma.poc.invocation-descriptor+xml`,
        `vnd.oma.poc.optimized-progress-report+xml`,
        `vnd.oma.push`,
        `vnd.oma.scidm.messages+xml`,
        `vnd.oma.xcap-directory+xml`,
        `vnd.omads-email+xml`,
        `vnd.omads-file+xml`,
        `vnd.omads-folder+xml`,
        `vnd.omaloc-supl-init`,
        `vnd.onepager`,
        `vnd.onepagertamp`,
        `vnd.onepagertamx`,
        `vnd.onepagertat`,
        `vnd.onepagertatp`,
        `vnd.onepagertatx`,
        `vnd.openblox.game+xml`,
        `vnd.openblox.game-binary`,
        `vnd.openeye.oeb`,
        `vnd.openofficeorg.extension`,
        `vnd.openstreetmap.data+xml`,
        `vnd.openxmlformats-officedocument.custom-properties+xml`,
        `vnd.openxmlformats-officedocument.customxmlproperties+xml`,
        `vnd.openxmlformats-officedocument.drawing+xml`,
        `vnd.openxmlformats-officedocument.drawingml.chart+xml`,
        `vnd.openxmlformats-officedocument.drawingml.chartshapes+xml`,
        `vnd.openxmlformats-officedocument.drawingml.diagramcolors+xml`,
        `vnd.openxmlformats-officedocument.drawingml.diagramdata+xml`,
        `vnd.openxmlformats-officedocument.drawingml.diagramlayout+xml`,
        `vnd.openxmlformats-officedocument.drawingml.diagramstyle+xml`,
        `vnd.openxmlformats-officedocument.extended-properties+xml`,
        `vnd.openxmlformats-officedocument.presentationml.commentauthors+xml`,
        `vnd.openxmlformats-officedocument.presentationml.comments+xml`,
        `vnd.openxmlformats-officedocument.presentationml.handoutmaster+xml`,
        `vnd.openxmlformats-officedocument.presentationml.notesmaster+xml`,
        `vnd.openxmlformats-officedocument.presentationml.notesslide+xml`,
        `vnd.openxmlformats-officedocument.presentationml.presentation`,
        `vnd.openxmlformats-officedocument.presentationml.presentation.main+xml`,
        `vnd.openxmlformats-officedocument.presentationml.presprops+xml`,
        `vnd.openxmlformats-officedocument.presentationml.slide`,
        `vnd.openxmlformats-officedocument.presentationml.slide+xml`,
        `vnd.openxmlformats-officedocument.presentationml.slidelayout+xml`,
        `vnd.openxmlformats-officedocument.presentationml.slidemaster+xml`,
        `vnd.openxmlformats-officedocument.presentationml.slideshow`,
        `vnd.openxmlformats-officedocument.presentationml.slideshow.main+xml`,
        `vnd.openxmlformats-officedocument.presentationml.slideupdateinfo+xml`,
        `vnd.openxmlformats-officedocument.presentationml.tablestyles+xml`,
        `vnd.openxmlformats-officedocument.presentationml.tags+xml`,
        `vnd.openxmlformats-officedocument.presentationml.template`,
        `vnd.openxmlformats-officedocument.presentationml.template.main+xml`,
        `vnd.openxmlformats-officedocument.presentationml.viewprops+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.calcchain+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.chartsheet+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.comments+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.connections+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.dialogsheet+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.externallink+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.pivotcachedefinition+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.pivotcacherecords+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.pivottable+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.querytable+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.revisionheaders+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.revisionlog+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.sharedstrings+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.sheet`,
        `vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.sheetmetadata+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.styles+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.table+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.tablesinglecells+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.template`,
        `vnd.openxmlformats-officedocument.spreadsheetml.template.main+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.usernames+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.volatiledependencies+xml`,
        `vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml`,
        `vnd.openxmlformats-officedocument.theme+xml`,
        `vnd.openxmlformats-officedocument.themeoverride+xml`,
        `vnd.openxmlformats-officedocument.vmldrawing`,
        `vnd.openxmlformats-officedocument.wordprocessingml.comments+xml`,
        `vnd.openxmlformats-officedocument.wordprocessingml.document`,
        `vnd.openxmlformats-officedocument.wordprocessingml.document.glossary+xml`,
        `vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml`,
        `vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml`,
        `vnd.openxmlformats-officedocument.wordprocessingml.fonttable+xml`,
        `vnd.openxmlformats-officedocument.wordprocessingml.footer+xml`,
        `vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml`,
        `vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml`,
        `vnd.openxmlformats-officedocument.wordprocessingml.settings+xml`,
        `vnd.openxmlformats-officedocument.wordprocessingml.styles+xml`,
        `vnd.openxmlformats-officedocument.wordprocessingml.template`,
        `vnd.openxmlformats-officedocument.wordprocessingml.template.main+xml`,
        `vnd.openxmlformats-officedocument.wordprocessingml.websettings+xml`,
        `vnd.openxmlformats-package.core-properties+xml`,
        `vnd.openxmlformats-package.digital-signature-xmlsignature+xml`,
        `vnd.openxmlformats-package.relationships+xml`,
        `vnd.oracle.resource+json`,
        `vnd.orange.indata`,
        `vnd.osa.netdeploy`,
        `vnd.osgeo.mapguide.package`,
        `vnd.osgi.bundle`,
        `vnd.osgi.dp`,
        `vnd.osgi.subsystem`,
        `vnd.otps.ct-kip+xml`,
        `vnd.oxli.countgraph`,
        `vnd.pagerduty+json`,
        `vnd.palm`,
        `vnd.panoply`,
        `vnd.paos.xml`,
        `vnd.patentdive`,
        `vnd.patientecommsdoc`,
        `vnd.pawaafile`,
        `vnd.pcos`,
        `vnd.pg.format`,
        `vnd.pg.osasli`,
        `vnd.piaccess.application-licence`,
        `vnd.picsel`,
        `vnd.pmi.widget`,
        `vnd.poc.group-advertisement+xml`,
        `vnd.pocketlearn`,
        `vnd.powerbuilder6`,
        `vnd.powerbuilder6-s`,
        `vnd.powerbuilder7`,
        `vnd.powerbuilder7-s`,
        `vnd.powerbuilder75`,
        `vnd.powerbuilder75-s`,
        `vnd.preminet`,
        `vnd.previewsystems.box`,
        `vnd.proteus.magazine`,
        `vnd.psfs`,
        `vnd.publishare-delta-tree`,
        `vnd.pvi.ptid1`,
        `vnd.pwg-multiplexed`,
        `vnd.pwg-xhtml-print+xml`,
        `vnd.qualcomm.brew-app-res`,
        `vnd.quarantainenet`,
        `vnd.quark.quarkxpress`,
        `vnd.quobject-quoxdocument`,
        `vnd.radisys.moml+xml`,
        `vnd.radisys.msml+xml`,
        `vnd.radisys.msml-audit+xml`,
        `vnd.radisys.msml-audit-conf+xml`,
        `vnd.radisys.msml-audit-conn+xml`,
        `vnd.radisys.msml-audit-dialog+xml`,
        `vnd.radisys.msml-audit-stream+xml`,
        `vnd.radisys.msml-conf+xml`,
        `vnd.radisys.msml-dialog+xml`,
        `vnd.radisys.msml-dialog-base+xml`,
        `vnd.radisys.msml-dialog-fax-detect+xml`,
        `vnd.radisys.msml-dialog-fax-sendrecv+xml`,
        `vnd.radisys.msml-dialog-group+xml`,
        `vnd.radisys.msml-dialog-speech+xml`,
        `vnd.radisys.msml-dialog-transform+xml`,
        `vnd.rainstor.data`,
        `vnd.rapid`,
        `vnd.rar`,
        `vnd.realvnc.bed`,
        `vnd.recordare.musicxml`,
        `vnd.recordare.musicxml+xml`,
        `vnd.renlearn.rlprint`,
        `vnd.resilient.logic`,
        `vnd.restful+json`,
        `vnd.rig.cryptonote`,
        `vnd.rim.cod`,
        `vnd.rn-realmedia`,
        `vnd.rn-realmedia-vbr`,
        `vnd.route66.link66+xml`,
        `vnd.rs-274x`,
        `vnd.ruckus.download`,
        `vnd.s3sms`,
        `vnd.sailingtracker.track`,
        `vnd.sar`,
        `vnd.sbm.cid`,
        `vnd.sbm.mid2`,
        `vnd.scribus`,
        `vnd.sealed.3df`,
        `vnd.sealed.csf`,
        `vnd.sealed.doc`,
        `vnd.sealed.eml`,
        `vnd.sealed.mht`,
        `vnd.sealed.net`,
        `vnd.sealed.ppt`,
        `vnd.sealed.tiff`,
        `vnd.sealed.xls`,
        `vnd.sealedmedia.softseal.html`,
        `vnd.sealedmedia.softseal.pdf`,
        `vnd.seemail`,
        `vnd.seis+json`,
        `vnd.sema`,
        `vnd.semd`,
        `vnd.semf`,
        `vnd.shade-save-file`,
        `vnd.shana.informed.formdata`,
        `vnd.shana.informed.formtemplate`,
        `vnd.shana.informed.interchange`,
        `vnd.shana.informed.package`,
        `vnd.shootproof+json`,
        `vnd.shopkick+json`,
        `vnd.shp`,
        `vnd.shx`,
        `vnd.sigrok.session`,
        `vnd.simtech-mindmapper`,
        `vnd.siren+json`,
        `vnd.smaf`,
        `vnd.smart.notebook`,
        `vnd.smart.teacher`,
        `vnd.snesdev-page-table`,
        `vnd.software602.filler.form+xml`,
        `vnd.software602.filler.form-xml-zip`,
        `vnd.solent.sdkm+xml`,
        `vnd.spotfire.dxp`,
        `vnd.spotfire.sfs`,
        `vnd.sqlite3`,
        `vnd.sss-cod`,
        `vnd.sss-dtf`,
        `vnd.sss-ntf`,
        `vnd.stardivision.calc`,
        `vnd.stardivision.draw`,
        `vnd.stardivision.impress`,
        `vnd.stardivision.math`,
        `vnd.stardivision.writer`,
        `vnd.stardivision.writer-global`,
        `vnd.stepmania.package`,
        `vnd.stepmania.stepchart`,
        `vnd.street-stream`,
        `vnd.sun.wadl+xml`,
        `vnd.sun.xml.calc`,
        `vnd.sun.xml.calc.template`,
        `vnd.sun.xml.draw`,
        `vnd.sun.xml.draw.template`,
        `vnd.sun.xml.impress`,
        `vnd.sun.xml.impress.template`,
        `vnd.sun.xml.math`,
        `vnd.sun.xml.writer`,
        `vnd.sun.xml.writer.global`,
        `vnd.sun.xml.writer.template`,
        `vnd.sus-calendar`,
        `vnd.svd`,
        `vnd.swiftview-ics`,
        `vnd.sycle+xml`,
        `vnd.symbian.install`,
        `vnd.syncml+xml`,
        `vnd.syncml.dm+wbxml`,
        `vnd.syncml.dm+xml`,
        `vnd.syncml.dm.notification`,
        `vnd.syncml.dmddf+wbxml`,
        `vnd.syncml.dmddf+xml`,
        `vnd.syncml.dmtnds+wbxml`,
        `vnd.syncml.dmtnds+xml`,
        `vnd.syncml.ds.notification`,
        `vnd.tableschema+json`,
        `vnd.tao.intent-module-archive`,
        `vnd.tcpdump.pcap`,
        `vnd.think-cell.ppttc+json`,
        `vnd.tmd.mediaflex.api+xml`,
        `vnd.tml`,
        `vnd.tmobile-livetv`,
        `vnd.tri.onesource`,
        `vnd.trid.tpt`,
        `vnd.triscape.mxs`,
        `vnd.trueapp`,
        `vnd.truedoc`,
        `vnd.ubisoft.webplayer`,
        `vnd.ufdl`,
        `vnd.uiq.theme`,
        `vnd.umajin`,
        `vnd.unity`,
        `vnd.uoml+xml`,
        `vnd.uplanet.alert`,
        `vnd.uplanet.alert-wbxml`,
        `vnd.uplanet.bearer-choice`,
        `vnd.uplanet.bearer-choice-wbxml`,
        `vnd.uplanet.cacheop`,
        `vnd.uplanet.cacheop-wbxml`,
        `vnd.uplanet.channel`,
        `vnd.uplanet.channel-wbxml`,
        `vnd.uplanet.list`,
        `vnd.uplanet.list-wbxml`,
        `vnd.uplanet.listcmd`,
        `vnd.uplanet.listcmd-wbxml`,
        `vnd.uplanet.signal`,
        `vnd.uri-map`,
        `vnd.valve.source.material`,
        `vnd.vcx`,
        `vnd.vd-study`,
        `vnd.vectorworks`,
        `vnd.vel+json`,
        `vnd.verimatrix.vcas`,
        `vnd.veryant.thin`,
        `vnd.ves.encrypted`,
        `vnd.vidsoft.vidconference`,
        `vnd.visio`,
        `vnd.visionary`,
        `vnd.vividence.scriptfile`,
        `vnd.vsf`,
        `vnd.wap.sic`,
        `vnd.wap.slc`,
        `vnd.wap.wbxml`,
        `vnd.wap.wmlc`,
        `vnd.wap.wmlscriptc`,
        `vnd.webturbo`,
        `vnd.wfa.dpp`,
        `vnd.wfa.p2p`,
        `vnd.wfa.wsc`,
        `vnd.windows.devicepairing`,
        `vnd.wmc`,
        `vnd.wmf.bootstrap`,
        `vnd.wolfram.mathematica`,
        `vnd.wolfram.mathematica.package`,
        `vnd.wolfram.player`,
        `vnd.wordperfect`,
        `vnd.wqd`,
        `vnd.wrq-hp3000-labelled`,
        `vnd.wt.stf`,
        `vnd.wv.csp+wbxml`,
        `vnd.wv.csp+xml`,
        `vnd.wv.ssp+xml`,
        `vnd.xacml+json`,
        `vnd.xara`,
        `vnd.xfdl`,
        `vnd.xfdl.webform`,
        `vnd.xmi+xml`,
        `vnd.xmpie.cpkg`,
        `vnd.xmpie.dpkg`,
        `vnd.xmpie.plan`,
        `vnd.xmpie.ppkg`,
        `vnd.xmpie.xlim`,
        `vnd.yamaha.hv-dic`,
        `vnd.yamaha.hv-script`,
        `vnd.yamaha.hv-voice`,
        `vnd.yamaha.openscoreformat`,
        `vnd.yamaha.openscoreformat.osfpvg+xml`,
        `vnd.yamaha.remote-setup`,
        `vnd.yamaha.smaf-audio`,
        `vnd.yamaha.smaf-phrase`,
        `vnd.yamaha.through-ngn`,
        `vnd.yamaha.tunnel-udpencap`,
        `vnd.yaoweme`,
        `vnd.yellowriver-custom-menu`,
        `vnd.youtube.yt`,
        `vnd.zul`,
        `vnd.zzazz.deck+xml`,
        `voicexml+xml`,
        `voucher-cms+json`,
        `vq-rtcpxr`,
        `wasm`,
        `watcherinfo+xml`,
        `webpush-options+json`,
        `whoispp-query`,
        `whoispp-response`,
        `widget`,
        `winhlp`,
        `wita`,
        `wordperfect5.1`,
        `wsdl+xml`,
        `wspolicy+xml`,
        `x-7z-compressed`,
        `x-abiword`,
        `x-ace-compressed`,
        `x-amf`,
        `x-apple-diskimage`,
        `x-arj`,
        `x-authorware-bin`,
        `x-authorware-map`,
        `x-authorware-seg`,
        `x-bcpio`,
        `x-bdoc`,
        `x-bittorrent`,
        `x-blorb`,
        `x-bzip`,
        `x-bzip2`,
        `x-cbr`,
        `x-cdlink`,
        `x-cfs-compressed`,
        `x-chat`,
        `x-chess-pgn`,
        `x-chrome-extension`,
        `x-cocoa`,
        `x-compress`,
        `x-conference`,
        `x-cpio`,
      )
    }
    trait application_3 {
      lazy val `x-csh`: MediaType                     =
        new MediaType("application", "x-csh", Compressible, NotBinary, List("csh"))
      lazy val `x-deb`: MediaType                     = new MediaType("application", "x-deb", Uncompressible, NotBinary)
      lazy val `x-debian-package`: MediaType          =
        new MediaType("application", "x-debian-package", Compressible, Binary, List("deb", "udeb"))
      lazy val `x-dgc-compressed`: MediaType          =
        new MediaType("application", "x-dgc-compressed", Compressible, NotBinary, List("dgc"))
      lazy val `x-director`: MediaType                = new MediaType(
        "application",
        "x-director",
        Compressible,
        NotBinary,
        List("dir", "dcr", "dxr", "cst", "cct", "cxt", "w3d", "fgd", "swa"),
      )
      lazy val `x-doom`: MediaType                    =
        new MediaType("application", "x-doom", Compressible, NotBinary, List("wad"))
      lazy val `x-dtbncx+xml`: MediaType              =
        new MediaType("application", "x-dtbncx+xml", Compressible, NotBinary, List("ncx"))
      lazy val `x-dtbook+xml`: MediaType              =
        new MediaType("application", "x-dtbook+xml", Compressible, NotBinary, List("dtb"))
      lazy val `x-dtbresource+xml`: MediaType         =
        new MediaType("application", "x-dtbresource+xml", Compressible, NotBinary, List("res"))
      lazy val `x-dvi`: MediaType                     =
        new MediaType("application", "x-dvi", Uncompressible, Binary, List("dvi"))
      lazy val `x-envoy`: MediaType                   =
        new MediaType("application", "x-envoy", Compressible, NotBinary, List("evy"))
      lazy val `x-eva`: MediaType                     =
        new MediaType("application", "x-eva", Compressible, NotBinary, List("eva"))
      lazy val `x-font-bdf`: MediaType                =
        new MediaType("application", "x-font-bdf", Compressible, NotBinary, List("bdf"))
      lazy val `x-font-dos`: MediaType                =
        new MediaType("application", "x-font-dos", Compressible, NotBinary)
      lazy val `x-font-framemaker`: MediaType         =
        new MediaType("application", "x-font-framemaker", Compressible, NotBinary)
      lazy val `x-font-ghostscript`: MediaType        =
        new MediaType("application", "x-font-ghostscript", Compressible, NotBinary, List("gsf"))
      lazy val `x-font-libgrx`: MediaType             =
        new MediaType("application", "x-font-libgrx", Compressible, NotBinary)
      lazy val `x-font-linux-psf`: MediaType          =
        new MediaType("application", "x-font-linux-psf", Compressible, NotBinary, List("psf"))
      lazy val `x-font-pcf`: MediaType                =
        new MediaType("application", "x-font-pcf", Compressible, NotBinary, List("pcf"))
      lazy val `x-font-snf`: MediaType                =
        new MediaType("application", "x-font-snf", Compressible, NotBinary, List("snf"))
      lazy val `x-font-speedo`: MediaType             =
        new MediaType("application", "x-font-speedo", Compressible, NotBinary)
      lazy val `x-font-sunos-news`: MediaType         =
        new MediaType("application", "x-font-sunos-news", Compressible, NotBinary)
      lazy val `x-font-type1`: MediaType              = new MediaType(
        "application",
        "x-font-type1",
        Compressible,
        NotBinary,
        List("pfa", "pfb", "pfm", "afm"),
      )
      lazy val `x-font-vfont`: MediaType              =
        new MediaType("application", "x-font-vfont", Compressible, NotBinary)
      lazy val `x-freearc`: MediaType                 =
        new MediaType("application", "x-freearc", Compressible, NotBinary, List("arc"))
      lazy val `x-futuresplash`: MediaType            =
        new MediaType("application", "x-futuresplash", Compressible, NotBinary, List("spl"))
      lazy val `x-gca-compressed`: MediaType          =
        new MediaType("application", "x-gca-compressed", Compressible, NotBinary, List("gca"))
      lazy val `x-glulx`: MediaType                   =
        new MediaType("application", "x-glulx", Compressible, NotBinary, List("ulx"))
      lazy val `x-gnumeric`: MediaType                =
        new MediaType("application", "x-gnumeric", Compressible, NotBinary, List("gnumeric"))
      lazy val `x-gramps-xml`: MediaType              =
        new MediaType("application", "x-gramps-xml", Compressible, NotBinary, List("gramps"))
      lazy val `x-gtar`: MediaType                    =
        new MediaType("application", "x-gtar", Compressible, Binary, List("gtar"))
      lazy val `x-gzip`: MediaType                    = new MediaType("application", "x-gzip", Compressible, Binary)
      lazy val `x-hdf`: MediaType                     =
        new MediaType("application", "x-hdf", Compressible, NotBinary, List("hdf"))
      lazy val `x-httpd-php`: MediaType               =
        new MediaType("application", "x-httpd-php", Compressible, NotBinary, List("php"))
      lazy val `x-install-instructions`: MediaType    = new MediaType(
        "application",
        "x-install-instructions",
        Compressible,
        NotBinary,
        List("install"),
      )
      lazy val `x-iso9660-image`: MediaType           =
        new MediaType("application", "x-iso9660-image", Compressible, NotBinary, List("iso"))
      lazy val `x-java-archive-diff`: MediaType       = new MediaType(
        "application",
        "x-java-archive-diff",
        Compressible,
        NotBinary,
        List("jardiff"),
      )
      lazy val `x-java-jnlp-file`: MediaType          =
        new MediaType("application", "x-java-jnlp-file", Uncompressible, NotBinary, List("jnlp"))
      lazy val `x-javascript`: MediaType              =
        new MediaType("application", "x-javascript", Compressible, NotBinary)
      lazy val `x-keepass2`: MediaType                =
        new MediaType("application", "x-keepass2", Compressible, NotBinary, List("kdbx"))
      lazy val `x-latex`: MediaType                   =
        new MediaType("application", "x-latex", Uncompressible, Binary, List("latex"))
      lazy val `x-lua-bytecode`: MediaType            =
        new MediaType("application", "x-lua-bytecode", Compressible, NotBinary, List("luac"))
      lazy val `x-lzh-compressed`: MediaType          = new MediaType(
        "application",
        "x-lzh-compressed",
        Compressible,
        NotBinary,
        List("lzh", "lha"),
      )
      lazy val `x-makeself`: MediaType                =
        new MediaType("application", "x-makeself", Compressible, NotBinary, List("run"))
      lazy val `x-mie`: MediaType                     =
        new MediaType("application", "x-mie", Compressible, NotBinary, List("mie"))
      lazy val `x-mobipocket-ebook`: MediaType        = new MediaType(
        "application",
        "x-mobipocket-ebook",
        Compressible,
        NotBinary,
        List("prc", "mobi"),
      )
      lazy val `x-mpegurl`: MediaType                 =
        new MediaType("application", "x-mpegurl", Uncompressible, NotBinary)
      lazy val `x-ms-application`: MediaType          = new MediaType(
        "application",
        "x-ms-application",
        Compressible,
        NotBinary,
        List("application"),
      )
      lazy val `x-ms-shortcut`: MediaType             =
        new MediaType("application", "x-ms-shortcut", Compressible, NotBinary, List("lnk"))
      lazy val `x-ms-wmd`: MediaType                  =
        new MediaType("application", "x-ms-wmd", Compressible, NotBinary, List("wmd"))
      lazy val `x-ms-wmz`: MediaType                  =
        new MediaType("application", "x-ms-wmz", Compressible, NotBinary, List("wmz"))
      lazy val `x-ms-xbap`: MediaType                 =
        new MediaType("application", "x-ms-xbap", Compressible, NotBinary, List("xbap"))
      lazy val `x-msaccess`: MediaType                =
        new MediaType("application", "x-msaccess", Compressible, NotBinary, List("mdb"))
      lazy val `x-msbinder`: MediaType                =
        new MediaType("application", "x-msbinder", Compressible, NotBinary, List("obd"))
      lazy val `x-mscardfile`: MediaType              =
        new MediaType("application", "x-mscardfile", Compressible, NotBinary, List("crd"))
      lazy val `x-msclip`: MediaType                  =
        new MediaType("application", "x-msclip", Compressible, NotBinary, List("clp"))
      lazy val `x-msdos-program`: MediaType           =
        new MediaType("application", "x-msdos-program", Compressible, NotBinary, List("exe"))
      lazy val `x-msdownload`: MediaType              = new MediaType(
        "application",
        "x-msdownload",
        Compressible,
        NotBinary,
        List("exe", "dll", "com", "bat", "msi"),
      )
      lazy val `x-msmediaview`: MediaType             = new MediaType(
        "application",
        "x-msmediaview",
        Compressible,
        NotBinary,
        List("mvb", "m13", "m14"),
      )
      lazy val `x-msmetafile`: MediaType              = new MediaType(
        "application",
        "x-msmetafile",
        Compressible,
        NotBinary,
        List("wmf", "wmz", "emf", "emz"),
      )
      lazy val `x-msmoney`: MediaType                 =
        new MediaType("application", "x-msmoney", Compressible, NotBinary, List("mny"))
      lazy val `x-mspublisher`: MediaType             =
        new MediaType("application", "x-mspublisher", Compressible, NotBinary, List("pub"))
      lazy val `x-msschedule`: MediaType              =
        new MediaType("application", "x-msschedule", Compressible, NotBinary, List("scd"))
      lazy val `x-msterminal`: MediaType              =
        new MediaType("application", "x-msterminal", Compressible, NotBinary, List("trm"))
      lazy val `x-mswrite`: MediaType                 =
        new MediaType("application", "x-mswrite", Compressible, NotBinary, List("wri"))
      lazy val `x-netcdf`: MediaType                  =
        new MediaType("application", "x-netcdf", Compressible, NotBinary, List("nc", "cdf"))
      lazy val `x-ns-proxy-autoconfig`: MediaType     =
        new MediaType("application", "x-ns-proxy-autoconfig", Compressible, NotBinary, List("pac"))
      lazy val `x-nzb`: MediaType                     =
        new MediaType("application", "x-nzb", Compressible, NotBinary, List("nzb"))
      lazy val `x-perl`: MediaType                    =
        new MediaType("application", "x-perl", Compressible, NotBinary, List("pl", "pm"))
      lazy val `x-pilot`: MediaType                   =
        new MediaType("application", "x-pilot", Compressible, NotBinary, List("prc", "pdb"))
      lazy val `x-pkcs12`: MediaType                  =
        new MediaType("application", "x-pkcs12", Uncompressible, NotBinary, List("p12", "pfx"))
      lazy val `x-pkcs7-certificates`: MediaType      = new MediaType(
        "application",
        "x-pkcs7-certificates",
        Compressible,
        NotBinary,
        List("p7b", "spc"),
      )
      lazy val `x-pkcs7-certreqresp`: MediaType       =
        new MediaType("application", "x-pkcs7-certreqresp", Compressible, NotBinary, List("p7r"))
      lazy val `x-pki-message`: MediaType             =
        new MediaType("application", "x-pki-message", Compressible, NotBinary)
      lazy val `x-rar-compressed`: MediaType          =
        new MediaType("application", "x-rar-compressed", Uncompressible, Binary, List("rar"))
      lazy val `x-redhat-package-manager`: MediaType  =
        new MediaType("application", "x-redhat-package-manager", Compressible, Binary, List("rpm"))
      lazy val `x-research-info-systems`: MediaType   = new MediaType(
        "application",
        "x-research-info-systems",
        Compressible,
        NotBinary,
        List("ris"),
      )
      lazy val `x-sea`: MediaType                     =
        new MediaType("application", "x-sea", Compressible, NotBinary, List("sea"))
      lazy val `x-sh`: MediaType                      =
        new MediaType("application", "x-sh", Compressible, NotBinary, List("sh"))
      lazy val `x-shar`: MediaType                    =
        new MediaType("application", "x-shar", Compressible, NotBinary, List("shar"))
      lazy val `x-shockwave-flash`: MediaType         =
        new MediaType("application", "x-shockwave-flash", Uncompressible, Binary, List("swf"))
      lazy val `x-silverlight-app`: MediaType         =
        new MediaType("application", "x-silverlight-app", Compressible, NotBinary, List("xap"))
      lazy val `x-sql`: MediaType                     =
        new MediaType("application", "x-sql", Compressible, NotBinary, List("sql"))
      lazy val `x-stuffit`: MediaType                 =
        new MediaType("application", "x-stuffit", Uncompressible, NotBinary, List("sit"))
      lazy val `x-stuffitx`: MediaType                =
        new MediaType("application", "x-stuffitx", Compressible, NotBinary, List("sitx"))
      lazy val `x-subrip`: MediaType                  =
        new MediaType("application", "x-subrip", Compressible, NotBinary, List("srt"))
      lazy val `x-sv4cpio`: MediaType                 =
        new MediaType("application", "x-sv4cpio", Compressible, NotBinary, List("sv4cpio"))
      lazy val `x-sv4crc`: MediaType                  =
        new MediaType("application", "x-sv4crc", Compressible, NotBinary, List("sv4crc"))
      lazy val `x-t3vm-image`: MediaType              =
        new MediaType("application", "x-t3vm-image", Compressible, NotBinary, List("t3"))
      lazy val `x-tads`: MediaType                    =
        new MediaType("application", "x-tads", Compressible, NotBinary, List("gam"))
      lazy val `x-tar`: MediaType                     =
        new MediaType("application", "x-tar", Compressible, Binary, List("tar"))
      lazy val `x-tcl`: MediaType                     =
        new MediaType("application", "x-tcl", Compressible, NotBinary, List("tcl", "tk"))
      lazy val `x-tex`: MediaType                     =
        new MediaType("application", "x-tex", Compressible, Binary, List("tex"))
      lazy val `x-tex-tfm`: MediaType                 =
        new MediaType("application", "x-tex-tfm", Compressible, NotBinary, List("tfm"))
      lazy val `x-texinfo`: MediaType                 =
        new MediaType("application", "x-texinfo", Compressible, Binary, List("texinfo", "texi"))
      lazy val `x-tgif`: MediaType                    =
        new MediaType("application", "x-tgif", Compressible, NotBinary, List("obj"))
      lazy val `x-ustar`: MediaType                   =
        new MediaType("application", "x-ustar", Compressible, NotBinary, List("ustar"))
      lazy val `x-virtualbox-hdd`: MediaType          =
        new MediaType("application", "x-virtualbox-hdd", Compressible, NotBinary, List("hdd"))
      lazy val `x-virtualbox-ova`: MediaType          =
        new MediaType("application", "x-virtualbox-ova", Compressible, NotBinary, List("ova"))
      lazy val `x-virtualbox-ovf`: MediaType          =
        new MediaType("application", "x-virtualbox-ovf", Compressible, NotBinary, List("ovf"))
      lazy val `x-virtualbox-vbox`: MediaType         =
        new MediaType("application", "x-virtualbox-vbox", Compressible, NotBinary, List("vbox"))
      lazy val `x-virtualbox-vbox-extpack`: MediaType = new MediaType(
        "application",
        "x-virtualbox-vbox-extpack",
        Uncompressible,
        NotBinary,
        List("vbox-extpack"),
      )
      lazy val `x-virtualbox-vdi`: MediaType          =
        new MediaType("application", "x-virtualbox-vdi", Compressible, NotBinary, List("vdi"))
      lazy val `x-virtualbox-vhd`: MediaType          =
        new MediaType("application", "x-virtualbox-vhd", Compressible, NotBinary, List("vhd"))
      lazy val `x-virtualbox-vmdk`: MediaType         =
        new MediaType("application", "x-virtualbox-vmdk", Compressible, NotBinary, List("vmdk"))
      lazy val `x-wais-source`: MediaType             =
        new MediaType("application", "x-wais-source", Compressible, NotBinary, List("src"))
      lazy val `x-web-app-manifest+json`: MediaType   = new MediaType(
        "application",
        "x-web-app-manifest+json",
        Compressible,
        NotBinary,
        List("webapp"),
      )
      lazy val `x-www-form-urlencoded`: MediaType     =
        new MediaType("application", "x-www-form-urlencoded", Compressible, NotBinary)
      lazy val `x-x509-ca-cert`: MediaType            = new MediaType(
        "application",
        "x-x509-ca-cert",
        Compressible,
        Binary,
        List("der", "crt", "pem"),
      )
      lazy val `x-x509-ca-ra-cert`: MediaType         =
        new MediaType("application", "x-x509-ca-ra-cert", Compressible, NotBinary)
      lazy val `x-x509-next-ca-cert`: MediaType       =
        new MediaType("application", "x-x509-next-ca-cert", Compressible, NotBinary)
      lazy val `x-xfig`: MediaType                    =
        new MediaType("application", "x-xfig", Compressible, NotBinary, List("fig"))
      lazy val `x-xliff+xml`: MediaType               =
        new MediaType("application", "x-xliff+xml", Compressible, NotBinary, List("xlf"))
      lazy val `x-xpinstall`: MediaType               =
        new MediaType("application", "x-xpinstall", Uncompressible, Binary, List("xpi"))
      lazy val `x-xz`: MediaType                      =
        new MediaType("application", "x-xz", Compressible, NotBinary, List("xz"))
      lazy val `x-zmachine`: MediaType                = new MediaType(
        "application",
        "x-zmachine",
        Compressible,
        NotBinary,
        List("z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8"),
      )
      lazy val `x400-bp`: MediaType                   =
        new MediaType("application", "x400-bp", Compressible, NotBinary)
      lazy val `xacml+xml`: MediaType                 =
        new MediaType("application", "xacml+xml", Compressible, NotBinary)
      lazy val `xaml+xml`: MediaType                  =
        new MediaType("application", "xaml+xml", Compressible, NotBinary, List("xaml"))
      lazy val `xcap-att+xml`: MediaType              =
        new MediaType("application", "xcap-att+xml", Compressible, NotBinary, List("xav"))
      lazy val `xcap-caps+xml`: MediaType             =
        new MediaType("application", "xcap-caps+xml", Compressible, NotBinary, List("xca"))
      lazy val `xcap-diff+xml`: MediaType             =
        new MediaType("application", "xcap-diff+xml", Compressible, NotBinary, List("xdf"))
      lazy val `xcap-el+xml`: MediaType               =
        new MediaType("application", "xcap-el+xml", Compressible, NotBinary, List("xel"))
      lazy val `xcap-error+xml`: MediaType            =
        new MediaType("application", "xcap-error+xml", Compressible, NotBinary)
      lazy val `xcap-ns+xml`: MediaType               =
        new MediaType("application", "xcap-ns+xml", Compressible, NotBinary, List("xns"))
      lazy val `xcon-conference-info+xml`: MediaType  =
        new MediaType("application", "xcon-conference-info+xml", Compressible, NotBinary)
      lazy val `xcon-conference-info-diff+xml`: MediaType =
        new MediaType("application", "xcon-conference-info-diff+xml", Compressible, NotBinary)
      lazy val `xenc+xml`: MediaType                      =
        new MediaType("application", "xenc+xml", Compressible, NotBinary, List("xenc"))
      lazy val `xhtml+xml`: MediaType                     =
        new MediaType("application", "xhtml+xml", Compressible, NotBinary, List("xhtml", "xht"))
      lazy val `xhtml-voice+xml`: MediaType               =
        new MediaType("application", "xhtml-voice+xml", Compressible, NotBinary)
      lazy val `xliff+xml`: MediaType                     =
        new MediaType("application", "xliff+xml", Compressible, NotBinary, List("xlf"))
      lazy val `xml`: MediaType                           = new MediaType(
        "application",
        "xml",
        Compressible,
        NotBinary,
        List("xml", "xsl", "xsd", "rng"),
      )
      lazy val `xml-dtd`: MediaType                       =
        new MediaType("application", "xml-dtd", Compressible, NotBinary, List("dtd"))
      lazy val `xml-external-parsed-entity`: MediaType    =
        new MediaType("application", "xml-external-parsed-entity", Compressible, NotBinary)
      lazy val `xml-patch+xml`: MediaType                 =
        new MediaType("application", "xml-patch+xml", Compressible, NotBinary)
      lazy val `xmpp+xml`: MediaType                      =
        new MediaType("application", "xmpp+xml", Compressible, NotBinary)
      lazy val `xop+xml`: MediaType                       =
        new MediaType("application", "xop+xml", Compressible, NotBinary, List("xop"))
      lazy val `xproc+xml`: MediaType                     =
        new MediaType("application", "xproc+xml", Compressible, NotBinary, List("xpl"))
      lazy val `xslt+xml`: MediaType                      =
        new MediaType("application", "xslt+xml", Compressible, NotBinary, List("xsl", "xslt"))
      lazy val `xspf+xml`: MediaType                      =
        new MediaType("application", "xspf+xml", Compressible, NotBinary, List("xspf"))
      lazy val `xv+xml`: MediaType                        = new MediaType(
        "application",
        "xv+xml",
        Compressible,
        NotBinary,
        List("mxml", "xhvml", "xvml", "xvm"),
      )
      lazy val `yang`: MediaType                          =
        new MediaType("application", "yang", Compressible, NotBinary, List("yang"))
      lazy val `yang-data+json`: MediaType                =
        new MediaType("application", "yang-data+json", Compressible, NotBinary)
      lazy val `yang-data+xml`: MediaType                 =
        new MediaType("application", "yang-data+xml", Compressible, NotBinary)
      lazy val `yang-patch+json`: MediaType               =
        new MediaType("application", "yang-patch+json", Compressible, NotBinary)
      lazy val `yang-patch+xml`: MediaType                =
        new MediaType("application", "yang-patch+xml", Compressible, NotBinary)
      lazy val `yin+xml`: MediaType                       =
        new MediaType("application", "yin+xml", Compressible, NotBinary, List("yin"))
      lazy val `zip`: MediaType                           =
        new MediaType("application", "zip", Uncompressible, Binary, List("zip"))
      lazy val `zlib`: MediaType       = new MediaType("application", "zlib", Compressible, NotBinary)
      lazy val `zstd`: MediaType       = new MediaType("application", "zstd", Compressible, NotBinary)
      lazy val part_3: List[MediaType] = List(
        `x-csh`,
        `x-deb`,
        `x-debian-package`,
        `x-dgc-compressed`,
        `x-director`,
        `x-doom`,
        `x-dtbncx+xml`,
        `x-dtbook+xml`,
        `x-dtbresource+xml`,
        `x-dvi`,
        `x-envoy`,
        `x-eva`,
        `x-font-bdf`,
        `x-font-dos`,
        `x-font-framemaker`,
        `x-font-ghostscript`,
        `x-font-libgrx`,
        `x-font-linux-psf`,
        `x-font-pcf`,
        `x-font-snf`,
        `x-font-speedo`,
        `x-font-sunos-news`,
        `x-font-type1`,
        `x-font-vfont`,
        `x-freearc`,
        `x-futuresplash`,
        `x-gca-compressed`,
        `x-glulx`,
        `x-gnumeric`,
        `x-gramps-xml`,
        `x-gtar`,
        `x-gzip`,
        `x-hdf`,
        `x-httpd-php`,
        `x-install-instructions`,
        `x-iso9660-image`,
        `x-java-archive-diff`,
        `x-java-jnlp-file`,
        `x-javascript`,
        `x-keepass2`,
        `x-latex`,
        `x-lua-bytecode`,
        `x-lzh-compressed`,
        `x-makeself`,
        `x-mie`,
        `x-mobipocket-ebook`,
        `x-mpegurl`,
        `x-ms-application`,
        `x-ms-shortcut`,
        `x-ms-wmd`,
        `x-ms-wmz`,
        `x-ms-xbap`,
        `x-msaccess`,
        `x-msbinder`,
        `x-mscardfile`,
        `x-msclip`,
        `x-msdos-program`,
        `x-msdownload`,
        `x-msmediaview`,
        `x-msmetafile`,
        `x-msmoney`,
        `x-mspublisher`,
        `x-msschedule`,
        `x-msterminal`,
        `x-mswrite`,
        `x-netcdf`,
        `x-ns-proxy-autoconfig`,
        `x-nzb`,
        `x-perl`,
        `x-pilot`,
        `x-pkcs12`,
        `x-pkcs7-certificates`,
        `x-pkcs7-certreqresp`,
        `x-pki-message`,
        `x-rar-compressed`,
        `x-redhat-package-manager`,
        `x-research-info-systems`,
        `x-sea`,
        `x-sh`,
        `x-shar`,
        `x-shockwave-flash`,
        `x-silverlight-app`,
        `x-sql`,
        `x-stuffit`,
        `x-stuffitx`,
        `x-subrip`,
        `x-sv4cpio`,
        `x-sv4crc`,
        `x-t3vm-image`,
        `x-tads`,
        `x-tar`,
        `x-tcl`,
        `x-tex`,
        `x-tex-tfm`,
        `x-texinfo`,
        `x-tgif`,
        `x-ustar`,
        `x-virtualbox-hdd`,
        `x-virtualbox-ova`,
        `x-virtualbox-ovf`,
        `x-virtualbox-vbox`,
        `x-virtualbox-vbox-extpack`,
        `x-virtualbox-vdi`,
        `x-virtualbox-vhd`,
        `x-virtualbox-vmdk`,
        `x-wais-source`,
        `x-web-app-manifest+json`,
        `x-www-form-urlencoded`,
        `x-x509-ca-cert`,
        `x-x509-ca-ra-cert`,
        `x-x509-next-ca-cert`,
        `x-xfig`,
        `x-xliff+xml`,
        `x-xpinstall`,
        `x-xz`,
        `x-zmachine`,
        `x400-bp`,
        `xacml+xml`,
        `xaml+xml`,
        `xcap-att+xml`,
        `xcap-caps+xml`,
        `xcap-diff+xml`,
        `xcap-el+xml`,
        `xcap-error+xml`,
        `xcap-ns+xml`,
        `xcon-conference-info+xml`,
        `xcon-conference-info-diff+xml`,
        `xenc+xml`,
        `xhtml+xml`,
        `xhtml-voice+xml`,
        `xliff+xml`,
        `xml`,
        `xml-dtd`,
        `xml-external-parsed-entity`,
        `xml-patch+xml`,
        `xmpp+xml`,
        `xop+xml`,
        `xproc+xml`,
        `xslt+xml`,
        `xspf+xml`,
        `xv+xml`,
        `yang`,
        `yang-data+json`,
        `yang-data+xml`,
        `yang-patch+json`,
        `yang-patch+xml`,
        `yin+xml`,
        `zip`,
        `zlib`,
        `zstd`,
      )
    }
  }
  object application
      extends application_parts.application_0
      with application_parts.application_1
      with application_parts.application_2
      with application_parts.application_3 {
    lazy val all: List[MediaType] = Nil ++ part_0 ++ part_1 ++ part_2 ++ part_3
  }
  object audio                             {
    lazy val `1d-interleaved-parityfec`: MediaType =
      new MediaType("audio", "1d-interleaved-parityfec", Compressible, Binary)
    lazy val `32kadpcm`: MediaType                 = new MediaType("audio", "32kadpcm", Compressible, Binary)
    lazy val `3gpp`: MediaType                     =
      new MediaType("audio", "3gpp", Uncompressible, Binary, List("3gpp"))
    lazy val `3gpp2`: MediaType                    = new MediaType("audio", "3gpp2", Compressible, Binary)
    lazy val `aac`: MediaType                      = new MediaType("audio", "aac", Compressible, Binary)
    lazy val `ac3`: MediaType                      = new MediaType("audio", "ac3", Compressible, Binary)
    lazy val `adpcm`: MediaType                    = new MediaType("audio", "adpcm", Compressible, Binary, List("adp"))
    lazy val `amr`: MediaType                      = new MediaType("audio", "amr", Compressible, Binary, List("amr"))
    lazy val `amr-wb`: MediaType                   = new MediaType("audio", "amr-wb", Compressible, Binary)
    lazy val `amr-wb+` : MediaType                 = new MediaType("audio", "amr-wb+", Compressible, Binary)
    lazy val `aptx`: MediaType                     = new MediaType("audio", "aptx", Compressible, Binary)
    lazy val `asc`: MediaType                      = new MediaType("audio", "asc", Compressible, Binary)
    lazy val `atrac-advanced-lossless`: MediaType  =
      new MediaType("audio", "atrac-advanced-lossless", Compressible, Binary)
    lazy val `atrac-x`: MediaType                  = new MediaType("audio", "atrac-x", Compressible, Binary)
    lazy val `atrac3`: MediaType                   = new MediaType("audio", "atrac3", Compressible, Binary)
    lazy val `basic`: MediaType                    =
      new MediaType("audio", "basic", Uncompressible, Binary, List("au", "snd"))
    lazy val `bv16`: MediaType                     = new MediaType("audio", "bv16", Compressible, Binary)
    lazy val `bv32`: MediaType                     = new MediaType("audio", "bv32", Compressible, Binary)
    lazy val `clearmode`: MediaType                = new MediaType("audio", "clearmode", Compressible, Binary)
    lazy val `cn`: MediaType                       = new MediaType("audio", "cn", Compressible, Binary)
    lazy val `dat12`: MediaType                    = new MediaType("audio", "dat12", Compressible, Binary)
    lazy val `dls`: MediaType                      = new MediaType("audio", "dls", Compressible, Binary)
    lazy val `dsr-es201108`: MediaType             =
      new MediaType("audio", "dsr-es201108", Compressible, Binary)
    lazy val `dsr-es202050`: MediaType             =
      new MediaType("audio", "dsr-es202050", Compressible, Binary)
    lazy val `dsr-es202211`: MediaType             =
      new MediaType("audio", "dsr-es202211", Compressible, Binary)
    lazy val `dsr-es202212`: MediaType             =
      new MediaType("audio", "dsr-es202212", Compressible, Binary)
    lazy val `dv`: MediaType                       = new MediaType("audio", "dv", Compressible, Binary)
    lazy val `dvi4`: MediaType                     = new MediaType("audio", "dvi4", Compressible, Binary)
    lazy val `eac3`: MediaType                     = new MediaType("audio", "eac3", Compressible, Binary)
    lazy val `encaprtp`: MediaType                 = new MediaType("audio", "encaprtp", Compressible, Binary)
    lazy val `evrc`: MediaType                     = new MediaType("audio", "evrc", Compressible, Binary)
    lazy val `evrc-qcp`: MediaType                 = new MediaType("audio", "evrc-qcp", Compressible, Binary)
    lazy val `evrc0`: MediaType                    = new MediaType("audio", "evrc0", Compressible, Binary)
    lazy val `evrc1`: MediaType                    = new MediaType("audio", "evrc1", Compressible, Binary)
    lazy val `evrcb`: MediaType                    = new MediaType("audio", "evrcb", Compressible, Binary)
    lazy val `evrcb0`: MediaType                   = new MediaType("audio", "evrcb0", Compressible, Binary)
    lazy val `evrcb1`: MediaType                   = new MediaType("audio", "evrcb1", Compressible, Binary)
    lazy val `evrcnw`: MediaType                   = new MediaType("audio", "evrcnw", Compressible, Binary)
    lazy val `evrcnw0`: MediaType                  = new MediaType("audio", "evrcnw0", Compressible, Binary)
    lazy val `evrcnw1`: MediaType                  = new MediaType("audio", "evrcnw1", Compressible, Binary)
    lazy val `evrcwb`: MediaType                   = new MediaType("audio", "evrcwb", Compressible, Binary)
    lazy val `evrcwb0`: MediaType                  = new MediaType("audio", "evrcwb0", Compressible, Binary)
    lazy val `evrcwb1`: MediaType                  = new MediaType("audio", "evrcwb1", Compressible, Binary)
    lazy val `evs`: MediaType                      = new MediaType("audio", "evs", Compressible, Binary)
    lazy val `flexfec`: MediaType                  = new MediaType("audio", "flexfec", Compressible, Binary)
    lazy val `fwdred`: MediaType                   = new MediaType("audio", "fwdred", Compressible, Binary)
    lazy val `g711-0`: MediaType                   = new MediaType("audio", "g711-0", Compressible, Binary)
    lazy val `g719`: MediaType                     = new MediaType("audio", "g719", Compressible, Binary)
    lazy val `g722`: MediaType                     = new MediaType("audio", "g722", Compressible, Binary)
    lazy val `g7221`: MediaType                    = new MediaType("audio", "g7221", Compressible, Binary)
    lazy val `g723`: MediaType                     = new MediaType("audio", "g723", Compressible, Binary)
    lazy val `g726-16`: MediaType                  = new MediaType("audio", "g726-16", Compressible, Binary)
    lazy val `g726-24`: MediaType                  = new MediaType("audio", "g726-24", Compressible, Binary)
    lazy val `g726-32`: MediaType                  = new MediaType("audio", "g726-32", Compressible, Binary)
    lazy val `g726-40`: MediaType                  = new MediaType("audio", "g726-40", Compressible, Binary)
    lazy val `g728`: MediaType                     = new MediaType("audio", "g728", Compressible, Binary)
    lazy val `g729`: MediaType                     = new MediaType("audio", "g729", Compressible, Binary)
    lazy val `g7291`: MediaType                    = new MediaType("audio", "g7291", Compressible, Binary)
    lazy val `g729d`: MediaType                    = new MediaType("audio", "g729d", Compressible, Binary)
    lazy val `g729e`: MediaType                    = new MediaType("audio", "g729e", Compressible, Binary)
    lazy val `gsm`: MediaType                      = new MediaType("audio", "gsm", Compressible, Binary)
    lazy val `gsm-efr`: MediaType                  = new MediaType("audio", "gsm-efr", Compressible, Binary)
    lazy val `gsm-hr-08`: MediaType                = new MediaType("audio", "gsm-hr-08", Compressible, Binary)
    lazy val `ilbc`: MediaType                     = new MediaType("audio", "ilbc", Compressible, Binary)
    lazy val `ip-mr_v2.5`: MediaType               = new MediaType("audio", "ip-mr_v2.5", Compressible, Binary)
    lazy val `isac`: MediaType                     = new MediaType("audio", "isac", Compressible, Binary)
    lazy val `l16`: MediaType                      = new MediaType("audio", "l16", Compressible, Binary)
    lazy val `l20`: MediaType                      = new MediaType("audio", "l20", Compressible, Binary)
    lazy val `l24`: MediaType                      = new MediaType("audio", "l24", Uncompressible, Binary)
    lazy val `l8`: MediaType                       = new MediaType("audio", "l8", Compressible, Binary)
    lazy val `lpc`: MediaType                      = new MediaType("audio", "lpc", Compressible, Binary)
    lazy val `melp`: MediaType                     = new MediaType("audio", "melp", Compressible, Binary)
    lazy val `melp1200`: MediaType                 = new MediaType("audio", "melp1200", Compressible, Binary)
    lazy val `melp2400`: MediaType                 = new MediaType("audio", "melp2400", Compressible, Binary)
    lazy val `melp600`: MediaType                  = new MediaType("audio", "melp600", Compressible, Binary)
    lazy val `mhas`: MediaType                     = new MediaType("audio", "mhas", Compressible, Binary)
    lazy val `midi`: MediaType                     =
      new MediaType("audio", "midi", Compressible, Binary, List("mid", "midi", "kar", "rmi"))
    lazy val `mobile-xmf`: MediaType               =
      new MediaType("audio", "mobile-xmf", Compressible, Binary, List("mxmf"))
    lazy val `mp3`: MediaType                      = new MediaType("audio", "mp3", Uncompressible, Binary, List("mp3"))
    lazy val `mp4`: MediaType                      =
      new MediaType("audio", "mp4", Uncompressible, Binary, List("m4a", "mp4a"))
    lazy val `mp4a-latm`: MediaType                = new MediaType("audio", "mp4a-latm", Compressible, Binary)
    lazy val `mpa`: MediaType                      = new MediaType("audio", "mpa", Compressible, Binary)
    lazy val `mpa-robust`: MediaType               = new MediaType("audio", "mpa-robust", Compressible, Binary)
    lazy val `mpeg`: MediaType                     = new MediaType(
      "audio",
      "mpeg",
      Uncompressible,
      Binary,
      List("mpga", "mp2", "mp2a", "mp3", "m2a", "m3a"),
    )
    lazy val `mpeg4-generic`: MediaType            =
      new MediaType("audio", "mpeg4-generic", Compressible, Binary)
    lazy val `musepack`: MediaType                 = new MediaType("audio", "musepack", Compressible, Binary)
    lazy val `ogg`: MediaType                      =
      new MediaType("audio", "ogg", Uncompressible, Binary, List("oga", "ogg", "spx", "opus"))
    lazy val `opus`: MediaType                     = new MediaType("audio", "opus", Compressible, Binary)
    lazy val `parityfec`: MediaType                = new MediaType("audio", "parityfec", Compressible, Binary)
    lazy val `pcma`: MediaType                     = new MediaType("audio", "pcma", Compressible, Binary)
    lazy val `pcma-wb`: MediaType                  = new MediaType("audio", "pcma-wb", Compressible, Binary)
    lazy val `pcmu`: MediaType                     = new MediaType("audio", "pcmu", Compressible, Binary)
    lazy val `pcmu-wb`: MediaType                  = new MediaType("audio", "pcmu-wb", Compressible, Binary)
    lazy val `prs.sid`: MediaType                  = new MediaType("audio", "prs.sid", Compressible, Binary)
    lazy val `qcelp`: MediaType                    = new MediaType("audio", "qcelp", Compressible, Binary)
    lazy val `raptorfec`: MediaType                = new MediaType("audio", "raptorfec", Compressible, Binary)
    lazy val `red`: MediaType                      = new MediaType("audio", "red", Compressible, Binary)
    lazy val `rtp-enc-aescm128`: MediaType         =
      new MediaType("audio", "rtp-enc-aescm128", Compressible, Binary)
    lazy val `rtp-midi`: MediaType                 = new MediaType("audio", "rtp-midi", Compressible, Binary)
    lazy val `rtploopback`: MediaType              = new MediaType("audio", "rtploopback", Compressible, Binary)
    lazy val `rtx`: MediaType                      = new MediaType("audio", "rtx", Compressible, Binary)
    lazy val `s3m`: MediaType                      = new MediaType("audio", "s3m", Compressible, Binary, List("s3m"))
    lazy val `scip`: MediaType                     = new MediaType("audio", "scip", Compressible, Binary)
    lazy val `silk`: MediaType                     = new MediaType("audio", "silk", Compressible, Binary, List("sil"))
    lazy val `smv`: MediaType                      = new MediaType("audio", "smv", Compressible, Binary)
    lazy val `smv-qcp`: MediaType                  = new MediaType("audio", "smv-qcp", Compressible, Binary)
    lazy val `smv0`: MediaType                     = new MediaType("audio", "smv0", Compressible, Binary)
    lazy val `sofa`: MediaType                     = new MediaType("audio", "sofa", Compressible, Binary)
    lazy val `sp-midi`: MediaType                  = new MediaType("audio", "sp-midi", Compressible, Binary)
    lazy val `speex`: MediaType                    = new MediaType("audio", "speex", Compressible, Binary)
    lazy val `t140c`: MediaType                    = new MediaType("audio", "t140c", Compressible, Binary)
    lazy val `t38`: MediaType                      = new MediaType("audio", "t38", Compressible, Binary)
    lazy val `telephone-event`: MediaType          =
      new MediaType("audio", "telephone-event", Compressible, Binary)
    lazy val `tetra_acelp`: MediaType              = new MediaType("audio", "tetra_acelp", Compressible, Binary)
    lazy val `tetra_acelp_bb`: MediaType           =
      new MediaType("audio", "tetra_acelp_bb", Compressible, Binary)
    lazy val `tone`: MediaType                     = new MediaType("audio", "tone", Compressible, Binary)
    lazy val `tsvcis`: MediaType                   = new MediaType("audio", "tsvcis", Compressible, Binary)
    lazy val `uemclip`: MediaType                  = new MediaType("audio", "uemclip", Compressible, Binary)
    lazy val `ulpfec`: MediaType                   = new MediaType("audio", "ulpfec", Compressible, Binary)
    lazy val `usac`: MediaType                     = new MediaType("audio", "usac", Compressible, Binary)
    lazy val `vdvi`: MediaType                     = new MediaType("audio", "vdvi", Compressible, Binary)
    lazy val `vmr-wb`: MediaType                   = new MediaType("audio", "vmr-wb", Compressible, Binary)
    lazy val `vnd.3gpp.iufp`: MediaType            =
      new MediaType("audio", "vnd.3gpp.iufp", Compressible, Binary)
    lazy val `vnd.4sb`: MediaType                  = new MediaType("audio", "vnd.4sb", Compressible, Binary)
    lazy val `vnd.audiokoz`: MediaType             =
      new MediaType("audio", "vnd.audiokoz", Compressible, Binary)
    lazy val `vnd.celp`: MediaType                 = new MediaType("audio", "vnd.celp", Compressible, Binary)
    lazy val `vnd.cisco.nse`: MediaType            =
      new MediaType("audio", "vnd.cisco.nse", Compressible, Binary)
    lazy val `vnd.cmles.radio-events`: MediaType   =
      new MediaType("audio", "vnd.cmles.radio-events", Compressible, Binary)
    lazy val `vnd.cns.anp1`: MediaType             =
      new MediaType("audio", "vnd.cns.anp1", Compressible, Binary)
    lazy val `vnd.cns.inf1`: MediaType             =
      new MediaType("audio", "vnd.cns.inf1", Compressible, Binary)
    lazy val `vnd.dece.audio`: MediaType           =
      new MediaType("audio", "vnd.dece.audio", Compressible, Binary, List("uva", "uvva"))
    lazy val `vnd.digital-winds`: MediaType        =
      new MediaType("audio", "vnd.digital-winds", Compressible, Binary, List("eol"))
    lazy val `vnd.dlna.adts`: MediaType            =
      new MediaType("audio", "vnd.dlna.adts", Compressible, Binary)
    lazy val `vnd.dolby.heaac.1`: MediaType        =
      new MediaType("audio", "vnd.dolby.heaac.1", Compressible, Binary)
    lazy val `vnd.dolby.heaac.2`: MediaType        =
      new MediaType("audio", "vnd.dolby.heaac.2", Compressible, Binary)
    lazy val `vnd.dolby.mlp`: MediaType            =
      new MediaType("audio", "vnd.dolby.mlp", Compressible, Binary)
    lazy val `vnd.dolby.mps`: MediaType            =
      new MediaType("audio", "vnd.dolby.mps", Compressible, Binary)
    lazy val `vnd.dolby.pl2`: MediaType            =
      new MediaType("audio", "vnd.dolby.pl2", Compressible, Binary)
    lazy val `vnd.dolby.pl2x`: MediaType           =
      new MediaType("audio", "vnd.dolby.pl2x", Compressible, Binary)
    lazy val `vnd.dolby.pl2z`: MediaType           =
      new MediaType("audio", "vnd.dolby.pl2z", Compressible, Binary)
    lazy val `vnd.dolby.pulse.1`: MediaType        =
      new MediaType("audio", "vnd.dolby.pulse.1", Compressible, Binary)
    lazy val `vnd.dra`: MediaType                  =
      new MediaType("audio", "vnd.dra", Compressible, Binary, List("dra"))
    lazy val `vnd.dts`: MediaType                  =
      new MediaType("audio", "vnd.dts", Compressible, Binary, List("dts"))
    lazy val `vnd.dts.hd`: MediaType               =
      new MediaType("audio", "vnd.dts.hd", Compressible, Binary, List("dtshd"))
    lazy val `vnd.dts.uhd`: MediaType              = new MediaType("audio", "vnd.dts.uhd", Compressible, Binary)
    lazy val `vnd.dvb.file`: MediaType             =
      new MediaType("audio", "vnd.dvb.file", Compressible, Binary)
    lazy val `vnd.everad.plj`: MediaType           =
      new MediaType("audio", "vnd.everad.plj", Compressible, Binary)
    lazy val `vnd.hns.audio`: MediaType            =
      new MediaType("audio", "vnd.hns.audio", Compressible, Binary)
    lazy val `vnd.lucent.voice`: MediaType         =
      new MediaType("audio", "vnd.lucent.voice", Compressible, Binary, List("lvp"))
    lazy val `vnd.ms-playready.media.pya`: MediaType    =
      new MediaType("audio", "vnd.ms-playready.media.pya", Compressible, Binary, List("pya"))
    lazy val `vnd.nokia.mobile-xmf`: MediaType          =
      new MediaType("audio", "vnd.nokia.mobile-xmf", Compressible, Binary)
    lazy val `vnd.nortel.vbk`: MediaType                =
      new MediaType("audio", "vnd.nortel.vbk", Compressible, Binary)
    lazy val `vnd.nuera.ecelp4800`: MediaType           =
      new MediaType("audio", "vnd.nuera.ecelp4800", Compressible, Binary, List("ecelp4800"))
    lazy val `vnd.nuera.ecelp7470`: MediaType           =
      new MediaType("audio", "vnd.nuera.ecelp7470", Compressible, Binary, List("ecelp7470"))
    lazy val `vnd.nuera.ecelp9600`: MediaType           =
      new MediaType("audio", "vnd.nuera.ecelp9600", Compressible, Binary, List("ecelp9600"))
    lazy val `vnd.octel.sbc`: MediaType                 =
      new MediaType("audio", "vnd.octel.sbc", Compressible, Binary)
    lazy val `vnd.presonus.multitrack`: MediaType       =
      new MediaType("audio", "vnd.presonus.multitrack", Compressible, Binary)
    lazy val `vnd.qcelp`: MediaType                     = new MediaType("audio", "vnd.qcelp", Compressible, Binary)
    lazy val `vnd.rhetorex.32kadpcm`: MediaType         =
      new MediaType("audio", "vnd.rhetorex.32kadpcm", Compressible, Binary)
    lazy val `vnd.rip`: MediaType                       =
      new MediaType("audio", "vnd.rip", Compressible, Binary, List("rip"))
    lazy val `vnd.rn-realaudio`: MediaType              =
      new MediaType("audio", "vnd.rn-realaudio", Uncompressible, Binary)
    lazy val `vnd.sealedmedia.softseal.mpeg`: MediaType =
      new MediaType("audio", "vnd.sealedmedia.softseal.mpeg", Compressible, Binary)
    lazy val `vnd.vmx.cvsd`: MediaType                  =
      new MediaType("audio", "vnd.vmx.cvsd", Compressible, Binary)
    lazy val `vnd.wave`: MediaType                      = new MediaType("audio", "vnd.wave", Uncompressible, Binary)
    lazy val `vorbis`: MediaType                        = new MediaType("audio", "vorbis", Uncompressible, Binary)
    lazy val `vorbis-config`: MediaType                 =
      new MediaType("audio", "vorbis-config", Compressible, Binary)
    lazy val `wav`: MediaType                   = new MediaType("audio", "wav", Uncompressible, Binary, List("wav"))
    lazy val `wave`: MediaType                  = new MediaType("audio", "wave", Uncompressible, Binary, List("wav"))
    lazy val `webm`: MediaType                  =
      new MediaType("audio", "webm", Uncompressible, Binary, List("weba"))
    lazy val `x-aac`: MediaType                 =
      new MediaType("audio", "x-aac", Uncompressible, Binary, List("aac"))
    lazy val `x-aiff`: MediaType                =
      new MediaType("audio", "x-aiff", Compressible, Binary, List("aif", "aiff", "aifc"))
    lazy val `x-caf`: MediaType                 =
      new MediaType("audio", "x-caf", Uncompressible, Binary, List("caf"))
    lazy val `x-flac`: MediaType                =
      new MediaType("audio", "x-flac", Compressible, Binary, List("flac"))
    lazy val `x-m4a`: MediaType                 = new MediaType("audio", "x-m4a", Compressible, Binary, List("m4a"))
    lazy val `x-matroska`: MediaType            =
      new MediaType("audio", "x-matroska", Compressible, Binary, List("mka"))
    lazy val `x-mpegurl`: MediaType             =
      new MediaType("audio", "x-mpegurl", Compressible, Binary, List("m3u"))
    lazy val `x-ms-wax`: MediaType              =
      new MediaType("audio", "x-ms-wax", Compressible, Binary, List("wax"))
    lazy val `x-ms-wma`: MediaType              =
      new MediaType("audio", "x-ms-wma", Compressible, Binary, List("wma"))
    lazy val `x-pn-realaudio`: MediaType        =
      new MediaType("audio", "x-pn-realaudio", Compressible, Binary, List("ram", "ra"))
    lazy val `x-pn-realaudio-plugin`: MediaType =
      new MediaType("audio", "x-pn-realaudio-plugin", Compressible, Binary, List("rmp"))
    lazy val `x-realaudio`: MediaType           =
      new MediaType("audio", "x-realaudio", Compressible, Binary, List("ra"))
    lazy val `x-tta`: MediaType                 = new MediaType("audio", "x-tta", Compressible, Binary)
    lazy val `x-wav`: MediaType                 = new MediaType("audio", "x-wav", Compressible, Binary, List("wav"))
    lazy val `xm`: MediaType                    = new MediaType("audio", "xm", Compressible, Binary, List("xm"))
    lazy val all: List[MediaType]               = List(
      `1d-interleaved-parityfec`,
      `32kadpcm`,
      `3gpp`,
      `3gpp2`,
      `aac`,
      `ac3`,
      `adpcm`,
      `amr`,
      `amr-wb`,
      `amr-wb+`,
      `aptx`,
      `asc`,
      `atrac-advanced-lossless`,
      `atrac-x`,
      `atrac3`,
      `basic`,
      `bv16`,
      `bv32`,
      `clearmode`,
      `cn`,
      `dat12`,
      `dls`,
      `dsr-es201108`,
      `dsr-es202050`,
      `dsr-es202211`,
      `dsr-es202212`,
      `dv`,
      `dvi4`,
      `eac3`,
      `encaprtp`,
      `evrc`,
      `evrc-qcp`,
      `evrc0`,
      `evrc1`,
      `evrcb`,
      `evrcb0`,
      `evrcb1`,
      `evrcnw`,
      `evrcnw0`,
      `evrcnw1`,
      `evrcwb`,
      `evrcwb0`,
      `evrcwb1`,
      `evs`,
      `flexfec`,
      `fwdred`,
      `g711-0`,
      `g719`,
      `g722`,
      `g7221`,
      `g723`,
      `g726-16`,
      `g726-24`,
      `g726-32`,
      `g726-40`,
      `g728`,
      `g729`,
      `g7291`,
      `g729d`,
      `g729e`,
      `gsm`,
      `gsm-efr`,
      `gsm-hr-08`,
      `ilbc`,
      `ip-mr_v2.5`,
      `isac`,
      `l16`,
      `l20`,
      `l24`,
      `l8`,
      `lpc`,
      `melp`,
      `melp1200`,
      `melp2400`,
      `melp600`,
      `mhas`,
      `midi`,
      `mobile-xmf`,
      `mp3`,
      `mp4`,
      `mp4a-latm`,
      `mpa`,
      `mpa-robust`,
      `mpeg`,
      `mpeg4-generic`,
      `musepack`,
      `ogg`,
      `opus`,
      `parityfec`,
      `pcma`,
      `pcma-wb`,
      `pcmu`,
      `pcmu-wb`,
      `prs.sid`,
      `qcelp`,
      `raptorfec`,
      `red`,
      `rtp-enc-aescm128`,
      `rtp-midi`,
      `rtploopback`,
      `rtx`,
      `s3m`,
      `scip`,
      `silk`,
      `smv`,
      `smv-qcp`,
      `smv0`,
      `sofa`,
      `sp-midi`,
      `speex`,
      `t140c`,
      `t38`,
      `telephone-event`,
      `tetra_acelp`,
      `tetra_acelp_bb`,
      `tone`,
      `tsvcis`,
      `uemclip`,
      `ulpfec`,
      `usac`,
      `vdvi`,
      `vmr-wb`,
      `vnd.3gpp.iufp`,
      `vnd.4sb`,
      `vnd.audiokoz`,
      `vnd.celp`,
      `vnd.cisco.nse`,
      `vnd.cmles.radio-events`,
      `vnd.cns.anp1`,
      `vnd.cns.inf1`,
      `vnd.dece.audio`,
      `vnd.digital-winds`,
      `vnd.dlna.adts`,
      `vnd.dolby.heaac.1`,
      `vnd.dolby.heaac.2`,
      `vnd.dolby.mlp`,
      `vnd.dolby.mps`,
      `vnd.dolby.pl2`,
      `vnd.dolby.pl2x`,
      `vnd.dolby.pl2z`,
      `vnd.dolby.pulse.1`,
      `vnd.dra`,
      `vnd.dts`,
      `vnd.dts.hd`,
      `vnd.dts.uhd`,
      `vnd.dvb.file`,
      `vnd.everad.plj`,
      `vnd.hns.audio`,
      `vnd.lucent.voice`,
      `vnd.ms-playready.media.pya`,
      `vnd.nokia.mobile-xmf`,
      `vnd.nortel.vbk`,
      `vnd.nuera.ecelp4800`,
      `vnd.nuera.ecelp7470`,
      `vnd.nuera.ecelp9600`,
      `vnd.octel.sbc`,
      `vnd.presonus.multitrack`,
      `vnd.qcelp`,
      `vnd.rhetorex.32kadpcm`,
      `vnd.rip`,
      `vnd.rn-realaudio`,
      `vnd.sealedmedia.softseal.mpeg`,
      `vnd.vmx.cvsd`,
      `vnd.wave`,
      `vorbis`,
      `vorbis-config`,
      `wav`,
      `wave`,
      `webm`,
      `x-aac`,
      `x-aiff`,
      `x-caf`,
      `x-flac`,
      `x-m4a`,
      `x-matroska`,
      `x-mpegurl`,
      `x-ms-wax`,
      `x-ms-wma`,
      `x-pn-realaudio`,
      `x-pn-realaudio-plugin`,
      `x-realaudio`,
      `x-tta`,
      `x-wav`,
      `xm`,
    )
  }
  object chemical                          {
    lazy val `x-cdx`: MediaType   =
      new MediaType("chemical", "x-cdx", Compressible, NotBinary, List("cdx"))
    lazy val `x-cif`: MediaType   =
      new MediaType("chemical", "x-cif", Compressible, NotBinary, List("cif"))
    lazy val `x-cmdf`: MediaType  =
      new MediaType("chemical", "x-cmdf", Compressible, NotBinary, List("cmdf"))
    lazy val `x-cml`: MediaType   =
      new MediaType("chemical", "x-cml", Compressible, NotBinary, List("cml"))
    lazy val `x-csml`: MediaType  =
      new MediaType("chemical", "x-csml", Compressible, NotBinary, List("csml"))
    lazy val `x-pdb`: MediaType   = new MediaType("chemical", "x-pdb", Compressible, NotBinary)
    lazy val `x-xyz`: MediaType   =
      new MediaType("chemical", "x-xyz", Compressible, NotBinary, List("xyz"))
    lazy val all: List[MediaType] =
      List(`x-cdx`, `x-cif`, `x-cmdf`, `x-cml`, `x-csml`, `x-pdb`, `x-xyz`)
  }
  object font                              {
    lazy val `collection`: MediaType =
      new MediaType("font", "collection", Compressible, NotBinary, List("ttc"))
    lazy val `otf`: MediaType        = new MediaType("font", "otf", Compressible, NotBinary, List("otf"))
    lazy val `sfnt`: MediaType       = new MediaType("font", "sfnt", Compressible, NotBinary)
    lazy val `ttf`: MediaType        = new MediaType("font", "ttf", Compressible, NotBinary, List("ttf"))
    lazy val `woff`: MediaType       =
      new MediaType("font", "woff", Compressible, NotBinary, List("woff"))
    lazy val `woff2`: MediaType      =
      new MediaType("font", "woff2", Compressible, NotBinary, List("woff2"))
    lazy val all: List[MediaType]    = List(`collection`, `otf`, `sfnt`, `ttf`, `woff`, `woff2`)
  }
  object image                             {
    lazy val `aces`: MediaType                = new MediaType("image", "aces", Compressible, Binary, List("exr"))
    lazy val `apng`: MediaType                =
      new MediaType("image", "apng", Uncompressible, Binary, List("apng"))
    lazy val `avci`: MediaType                = new MediaType("image", "avci", Compressible, Binary)
    lazy val `avcs`: MediaType                = new MediaType("image", "avcs", Compressible, Binary)
    lazy val `avif`: MediaType                =
      new MediaType("image", "avif", Uncompressible, Binary, List("avif"))
    lazy val `bmp`: MediaType                 = new MediaType("image", "bmp", Compressible, Binary, List("bmp"))
    lazy val `cgm`: MediaType                 = new MediaType("image", "cgm", Compressible, Binary, List("cgm"))
    lazy val `dicom-rle`: MediaType           =
      new MediaType("image", "dicom-rle", Compressible, Binary, List("drle"))
    lazy val `emf`: MediaType                 = new MediaType("image", "emf", Compressible, Binary, List("emf"))
    lazy val `fits`: MediaType                = new MediaType("image", "fits", Compressible, Binary, List("fits"))
    lazy val `g3fax`: MediaType               = new MediaType("image", "g3fax", Compressible, Binary, List("g3"))
    lazy val `gif`: MediaType                 = new MediaType("image", "gif", Uncompressible, Binary, List("gif"))
    lazy val `heic`: MediaType                = new MediaType("image", "heic", Compressible, Binary, List("heic"))
    lazy val `heic-sequence`: MediaType       =
      new MediaType("image", "heic-sequence", Compressible, Binary, List("heics"))
    lazy val `heif`: MediaType                = new MediaType("image", "heif", Compressible, Binary, List("heif"))
    lazy val `heif-sequence`: MediaType       =
      new MediaType("image", "heif-sequence", Compressible, Binary, List("heifs"))
    lazy val `hej2k`: MediaType               =
      new MediaType("image", "hej2k", Compressible, Binary, List("hej2"))
    lazy val `hsj2`: MediaType                = new MediaType("image", "hsj2", Compressible, Binary, List("hsj2"))
    lazy val `ief`: MediaType                 = new MediaType("image", "ief", Compressible, Binary, List("ief"))
    lazy val `jls`: MediaType                 = new MediaType("image", "jls", Compressible, Binary, List("jls"))
    lazy val `jp2`: MediaType                 =
      new MediaType("image", "jp2", Uncompressible, Binary, List("jp2", "jpg2"))
    lazy val `jpeg`: MediaType                =
      new MediaType("image", "jpeg", Uncompressible, Binary, List("jpeg", "jpg", "jpe"))
    lazy val `jph`: MediaType                 = new MediaType("image", "jph", Compressible, Binary, List("jph"))
    lazy val `jphc`: MediaType                = new MediaType("image", "jphc", Compressible, Binary, List("jhc"))
    lazy val `jpm`: MediaType                 = new MediaType("image", "jpm", Uncompressible, Binary, List("jpm"))
    lazy val `jpx`: MediaType                 =
      new MediaType("image", "jpx", Uncompressible, Binary, List("jpx", "jpf"))
    lazy val `jxr`: MediaType                 = new MediaType("image", "jxr", Compressible, Binary, List("jxr"))
    lazy val `jxra`: MediaType                = new MediaType("image", "jxra", Compressible, Binary, List("jxra"))
    lazy val `jxrs`: MediaType                = new MediaType("image", "jxrs", Compressible, Binary, List("jxrs"))
    lazy val `jxs`: MediaType                 = new MediaType("image", "jxs", Compressible, Binary, List("jxs"))
    lazy val `jxsc`: MediaType                = new MediaType("image", "jxsc", Compressible, Binary, List("jxsc"))
    lazy val `jxsi`: MediaType                = new MediaType("image", "jxsi", Compressible, Binary, List("jxsi"))
    lazy val `jxss`: MediaType                = new MediaType("image", "jxss", Compressible, Binary, List("jxss"))
    lazy val `ktx`: MediaType                 = new MediaType("image", "ktx", Compressible, Binary, List("ktx"))
    lazy val `ktx2`: MediaType                = new MediaType("image", "ktx2", Compressible, Binary, List("ktx2"))
    lazy val `naplps`: MediaType              = new MediaType("image", "naplps", Compressible, Binary)
    lazy val `pjpeg`: MediaType               = new MediaType("image", "pjpeg", Uncompressible, Binary)
    lazy val `png`: MediaType                 = new MediaType("image", "png", Uncompressible, Binary, List("png"))
    lazy val `prs.btif`: MediaType            =
      new MediaType("image", "prs.btif", Compressible, Binary, List("btif"))
    lazy val `prs.pti`: MediaType             =
      new MediaType("image", "prs.pti", Compressible, Binary, List("pti"))
    lazy val `pwg-raster`: MediaType          = new MediaType("image", "pwg-raster", Compressible, Binary)
    lazy val `sgi`: MediaType                 = new MediaType("image", "sgi", Compressible, Binary, List("sgi"))
    lazy val `svg+xml`: MediaType             =
      new MediaType("image", "svg+xml", Compressible, Binary, List("svg", "svgz"))
    lazy val `t38`: MediaType                 = new MediaType("image", "t38", Compressible, Binary, List("t38"))
    lazy val `tiff`: MediaType                =
      new MediaType("image", "tiff", Uncompressible, Binary, List("tif", "tiff"))
    lazy val `tiff-fx`: MediaType             =
      new MediaType("image", "tiff-fx", Compressible, Binary, List("tfx"))
    lazy val `vnd.adobe.photoshop`: MediaType =
      new MediaType("image", "vnd.adobe.photoshop", Compressible, Binary, List("psd"))
    lazy val `vnd.airzip.accelerator.azv`: MediaType   =
      new MediaType("image", "vnd.airzip.accelerator.azv", Compressible, Binary, List("azv"))
    lazy val `vnd.cns.inf2`: MediaType                 =
      new MediaType("image", "vnd.cns.inf2", Compressible, Binary)
    lazy val `vnd.dece.graphic`: MediaType             = new MediaType(
      "image",
      "vnd.dece.graphic",
      Compressible,
      Binary,
      List("uvi", "uvvi", "uvg", "uvvg"),
    )
    lazy val `vnd.djvu`: MediaType                     =
      new MediaType("image", "vnd.djvu", Compressible, Binary, List("djvu", "djv"))
    lazy val `vnd.dvb.subtitle`: MediaType             =
      new MediaType("image", "vnd.dvb.subtitle", Compressible, Binary, List("sub"))
    lazy val `vnd.dwg`: MediaType                      =
      new MediaType("image", "vnd.dwg", Compressible, Binary, List("dwg"))
    lazy val `vnd.dxf`: MediaType                      =
      new MediaType("image", "vnd.dxf", Compressible, Binary, List("dxf"))
    lazy val `vnd.fastbidsheet`: MediaType             =
      new MediaType("image", "vnd.fastbidsheet", Compressible, Binary, List("fbs"))
    lazy val `vnd.fpx`: MediaType                      =
      new MediaType("image", "vnd.fpx", Compressible, Binary, List("fpx"))
    lazy val `vnd.fst`: MediaType                      =
      new MediaType("image", "vnd.fst", Compressible, Binary, List("fst"))
    lazy val `vnd.fujixerox.edmics-mmr`: MediaType     =
      new MediaType("image", "vnd.fujixerox.edmics-mmr", Compressible, Binary, List("mmr"))
    lazy val `vnd.fujixerox.edmics-rlc`: MediaType     =
      new MediaType("image", "vnd.fujixerox.edmics-rlc", Compressible, Binary, List("rlc"))
    lazy val `vnd.globalgraphics.pgb`: MediaType       =
      new MediaType("image", "vnd.globalgraphics.pgb", Compressible, Binary)
    lazy val `vnd.microsoft.icon`: MediaType           =
      new MediaType("image", "vnd.microsoft.icon", Compressible, Binary, List("ico"))
    lazy val `vnd.mix`: MediaType                      = new MediaType("image", "vnd.mix", Compressible, Binary)
    lazy val `vnd.mozilla.apng`: MediaType             =
      new MediaType("image", "vnd.mozilla.apng", Compressible, Binary)
    lazy val `vnd.ms-dds`: MediaType                   =
      new MediaType("image", "vnd.ms-dds", Compressible, Binary, List("dds"))
    lazy val `vnd.ms-modi`: MediaType                  =
      new MediaType("image", "vnd.ms-modi", Compressible, Binary, List("mdi"))
    lazy val `vnd.ms-photo`: MediaType                 =
      new MediaType("image", "vnd.ms-photo", Compressible, Binary, List("wdp"))
    lazy val `vnd.net-fpx`: MediaType                  =
      new MediaType("image", "vnd.net-fpx", Compressible, Binary, List("npx"))
    lazy val `vnd.pco.b16`: MediaType                  =
      new MediaType("image", "vnd.pco.b16", Compressible, Binary, List("b16"))
    lazy val `vnd.radiance`: MediaType                 =
      new MediaType("image", "vnd.radiance", Compressible, Binary)
    lazy val `vnd.sealed.png`: MediaType               =
      new MediaType("image", "vnd.sealed.png", Compressible, Binary)
    lazy val `vnd.sealedmedia.softseal.gif`: MediaType =
      new MediaType("image", "vnd.sealedmedia.softseal.gif", Compressible, Binary)
    lazy val `vnd.sealedmedia.softseal.jpg`: MediaType =
      new MediaType("image", "vnd.sealedmedia.softseal.jpg", Compressible, Binary)
    lazy val `vnd.svf`: MediaType                      = new MediaType("image", "vnd.svf", Compressible, Binary)
    lazy val `vnd.tencent.tap`: MediaType              =
      new MediaType("image", "vnd.tencent.tap", Compressible, Binary, List("tap"))
    lazy val `vnd.valve.source.texture`: MediaType     =
      new MediaType("image", "vnd.valve.source.texture", Compressible, Binary, List("vtf"))
    lazy val `vnd.wap.wbmp`: MediaType                 =
      new MediaType("image", "vnd.wap.wbmp", Compressible, Binary, List("wbmp"))
    lazy val `vnd.xiff`: MediaType                     =
      new MediaType("image", "vnd.xiff", Compressible, Binary, List("xif"))
    lazy val `vnd.zbrush.pcx`: MediaType               =
      new MediaType("image", "vnd.zbrush.pcx", Compressible, Binary, List("pcx"))
    lazy val `webp`: MediaType               = new MediaType("image", "webp", Compressible, Binary, List("webp"))
    lazy val `wmf`: MediaType                = new MediaType("image", "wmf", Compressible, Binary, List("wmf"))
    lazy val `x-3ds`: MediaType              = new MediaType("image", "x-3ds", Compressible, Binary, List("3ds"))
    lazy val `x-cmu-raster`: MediaType       =
      new MediaType("image", "x-cmu-raster", Compressible, Binary, List("ras"))
    lazy val `x-cmx`: MediaType              = new MediaType("image", "x-cmx", Compressible, Binary, List("cmx"))
    lazy val `x-freehand`: MediaType         = new MediaType(
      "image",
      "x-freehand",
      Compressible,
      Binary,
      List("fh", "fhc", "fh4", "fh5", "fh7"),
    )
    lazy val `x-icon`: MediaType             =
      new MediaType("image", "x-icon", Compressible, Binary, List("ico"))
    lazy val `x-jng`: MediaType              = new MediaType("image", "x-jng", Compressible, Binary, List("jng"))
    lazy val `x-mrsid-image`: MediaType      =
      new MediaType("image", "x-mrsid-image", Compressible, Binary, List("sid"))
    lazy val `x-ms-bmp`: MediaType           =
      new MediaType("image", "x-ms-bmp", Compressible, Binary, List("bmp"))
    lazy val `x-pcx`: MediaType              = new MediaType("image", "x-pcx", Compressible, Binary, List("pcx"))
    lazy val `x-pict`: MediaType             =
      new MediaType("image", "x-pict", Compressible, Binary, List("pic", "pct"))
    lazy val `x-portable-anymap`: MediaType  =
      new MediaType("image", "x-portable-anymap", Compressible, Binary, List("pnm"))
    lazy val `x-portable-bitmap`: MediaType  =
      new MediaType("image", "x-portable-bitmap", Compressible, Binary, List("pbm"))
    lazy val `x-portable-graymap`: MediaType =
      new MediaType("image", "x-portable-graymap", Compressible, Binary, List("pgm"))
    lazy val `x-portable-pixmap`: MediaType  =
      new MediaType("image", "x-portable-pixmap", Compressible, Binary, List("ppm"))
    lazy val `x-rgb`: MediaType              = new MediaType("image", "x-rgb", Compressible, Binary, List("rgb"))
    lazy val `x-tga`: MediaType              = new MediaType("image", "x-tga", Compressible, Binary, List("tga"))
    lazy val `x-xbitmap`: MediaType          =
      new MediaType("image", "x-xbitmap", Compressible, Binary, List("xbm"))
    lazy val `x-xcf`: MediaType              = new MediaType("image", "x-xcf", Uncompressible, Binary)
    lazy val `x-xpixmap`: MediaType          =
      new MediaType("image", "x-xpixmap", Compressible, Binary, List("xpm"))
    lazy val `x-xwindowdump`: MediaType      =
      new MediaType("image", "x-xwindowdump", Compressible, Binary, List("xwd"))
    lazy val all: List[MediaType]            = List(
      `aces`,
      `apng`,
      `avci`,
      `avcs`,
      `avif`,
      `bmp`,
      `cgm`,
      `dicom-rle`,
      `emf`,
      `fits`,
      `g3fax`,
      `gif`,
      `heic`,
      `heic-sequence`,
      `heif`,
      `heif-sequence`,
      `hej2k`,
      `hsj2`,
      `ief`,
      `jls`,
      `jp2`,
      `jpeg`,
      `jph`,
      `jphc`,
      `jpm`,
      `jpx`,
      `jxr`,
      `jxra`,
      `jxrs`,
      `jxs`,
      `jxsc`,
      `jxsi`,
      `jxss`,
      `ktx`,
      `ktx2`,
      `naplps`,
      `pjpeg`,
      `png`,
      `prs.btif`,
      `prs.pti`,
      `pwg-raster`,
      `sgi`,
      `svg+xml`,
      `t38`,
      `tiff`,
      `tiff-fx`,
      `vnd.adobe.photoshop`,
      `vnd.airzip.accelerator.azv`,
      `vnd.cns.inf2`,
      `vnd.dece.graphic`,
      `vnd.djvu`,
      `vnd.dvb.subtitle`,
      `vnd.dwg`,
      `vnd.dxf`,
      `vnd.fastbidsheet`,
      `vnd.fpx`,
      `vnd.fst`,
      `vnd.fujixerox.edmics-mmr`,
      `vnd.fujixerox.edmics-rlc`,
      `vnd.globalgraphics.pgb`,
      `vnd.microsoft.icon`,
      `vnd.mix`,
      `vnd.mozilla.apng`,
      `vnd.ms-dds`,
      `vnd.ms-modi`,
      `vnd.ms-photo`,
      `vnd.net-fpx`,
      `vnd.pco.b16`,
      `vnd.radiance`,
      `vnd.sealed.png`,
      `vnd.sealedmedia.softseal.gif`,
      `vnd.sealedmedia.softseal.jpg`,
      `vnd.svf`,
      `vnd.tencent.tap`,
      `vnd.valve.source.texture`,
      `vnd.wap.wbmp`,
      `vnd.xiff`,
      `vnd.zbrush.pcx`,
      `webp`,
      `wmf`,
      `x-3ds`,
      `x-cmu-raster`,
      `x-cmx`,
      `x-freehand`,
      `x-icon`,
      `x-jng`,
      `x-mrsid-image`,
      `x-ms-bmp`,
      `x-pcx`,
      `x-pict`,
      `x-portable-anymap`,
      `x-portable-bitmap`,
      `x-portable-graymap`,
      `x-portable-pixmap`,
      `x-rgb`,
      `x-tga`,
      `x-xbitmap`,
      `x-xcf`,
      `x-xpixmap`,
      `x-xwindowdump`,
    )
  }
  object message                           {
    lazy val `cpim`: MediaType                            = new MediaType("message", "cpim", Compressible, NotBinary)
    lazy val `delivery-status`: MediaType                 =
      new MediaType("message", "delivery-status", Compressible, NotBinary)
    lazy val `disposition-notification`: MediaType        = new MediaType(
      "message",
      "disposition-notification",
      Compressible,
      NotBinary,
      List("disposition-notification"),
    )
    lazy val `external-body`: MediaType                   =
      new MediaType("message", "external-body", Compressible, NotBinary)
    lazy val `feedback-report`: MediaType                 =
      new MediaType("message", "feedback-report", Compressible, NotBinary)
    lazy val `global`: MediaType                          =
      new MediaType("message", "global", Compressible, NotBinary, List("u8msg"))
    lazy val `global-delivery-status`: MediaType          =
      new MediaType("message", "global-delivery-status", Compressible, NotBinary, List("u8dsn"))
    lazy val `global-disposition-notification`: MediaType = new MediaType(
      "message",
      "global-disposition-notification",
      Compressible,
      NotBinary,
      List("u8mdn"),
    )
    lazy val `global-headers`: MediaType                  =
      new MediaType("message", "global-headers", Compressible, NotBinary, List("u8hdr"))
    lazy val `http`: MediaType                            = new MediaType("message", "http", Uncompressible, NotBinary)
    lazy val `imdn+xml`: MediaType        = new MediaType("message", "imdn+xml", Compressible, NotBinary)
    lazy val `news`: MediaType            = new MediaType("message", "news", Compressible, NotBinary)
    lazy val `partial`: MediaType         = new MediaType("message", "partial", Uncompressible, NotBinary)
    lazy val `rfc822`: MediaType          =
      new MediaType("message", "rfc822", Compressible, NotBinary, List("eml", "mime"))
    lazy val `s-http`: MediaType          = new MediaType("message", "s-http", Compressible, NotBinary)
    lazy val `sip`: MediaType             = new MediaType("message", "sip", Compressible, NotBinary)
    lazy val `sipfrag`: MediaType         = new MediaType("message", "sipfrag", Compressible, NotBinary)
    lazy val `tracking-status`: MediaType =
      new MediaType("message", "tracking-status", Compressible, NotBinary)
    lazy val `vnd.si.simp`: MediaType     =
      new MediaType("message", "vnd.si.simp", Compressible, NotBinary)
    lazy val `vnd.wfa.wsc`: MediaType     =
      new MediaType("message", "vnd.wfa.wsc", Compressible, NotBinary, List("wsc"))
    lazy val all: List[MediaType]         = List(
      `cpim`,
      `delivery-status`,
      `disposition-notification`,
      `external-body`,
      `feedback-report`,
      `global`,
      `global-delivery-status`,
      `global-disposition-notification`,
      `global-headers`,
      `http`,
      `imdn+xml`,
      `news`,
      `partial`,
      `rfc822`,
      `s-http`,
      `sip`,
      `sipfrag`,
      `tracking-status`,
      `vnd.si.simp`,
      `vnd.wfa.wsc`,
    )
  }
  object model                             {
    lazy val `3mf`: MediaType               = new MediaType("model", "3mf", Compressible, NotBinary, List("3mf"))
    lazy val `e57`: MediaType               = new MediaType("model", "e57", Compressible, NotBinary)
    lazy val `gltf+json`: MediaType         =
      new MediaType("model", "gltf+json", Compressible, NotBinary, List("gltf"))
    lazy val `gltf-binary`: MediaType       =
      new MediaType("model", "gltf-binary", Compressible, NotBinary, List("glb"))
    lazy val `iges`: MediaType              =
      new MediaType("model", "iges", Uncompressible, NotBinary, List("igs", "iges"))
    lazy val `mesh`: MediaType              =
      new MediaType("model", "mesh", Uncompressible, NotBinary, List("msh", "mesh", "silo"))
    lazy val `mtl`: MediaType               = new MediaType("model", "mtl", Compressible, NotBinary, List("mtl"))
    lazy val `obj`: MediaType               = new MediaType("model", "obj", Compressible, NotBinary, List("obj"))
    lazy val `stl`: MediaType               = new MediaType("model", "stl", Compressible, NotBinary, List("stl"))
    lazy val `vnd.collada+xml`: MediaType   =
      new MediaType("model", "vnd.collada+xml", Compressible, NotBinary, List("dae"))
    lazy val `vnd.dwf`: MediaType           =
      new MediaType("model", "vnd.dwf", Compressible, NotBinary, List("dwf"))
    lazy val `vnd.flatland.3dml`: MediaType =
      new MediaType("model", "vnd.flatland.3dml", Compressible, NotBinary)
    lazy val `vnd.gdl`: MediaType           =
      new MediaType("model", "vnd.gdl", Compressible, NotBinary, List("gdl"))
    lazy val `vnd.gs-gdl`: MediaType        = new MediaType("model", "vnd.gs-gdl", Compressible, NotBinary)
    lazy val `vnd.gs.gdl`: MediaType        = new MediaType("model", "vnd.gs.gdl", Compressible, NotBinary)
    lazy val `vnd.gtw`: MediaType           =
      new MediaType("model", "vnd.gtw", Compressible, NotBinary, List("gtw"))
    lazy val `vnd.moml+xml`: MediaType      =
      new MediaType("model", "vnd.moml+xml", Compressible, NotBinary)
    lazy val `vnd.mts`: MediaType           =
      new MediaType("model", "vnd.mts", Compressible, NotBinary, List("mts"))
    lazy val `vnd.opengex`: MediaType       =
      new MediaType("model", "vnd.opengex", Compressible, NotBinary, List("ogex"))
    lazy val `vnd.parasolid.transmit.binary`: MediaType    =
      new MediaType("model", "vnd.parasolid.transmit.binary", Compressible, NotBinary, List("x_b"))
    lazy val `vnd.parasolid.transmit.text`: MediaType      =
      new MediaType("model", "vnd.parasolid.transmit.text", Compressible, NotBinary, List("x_t"))
    lazy val `vnd.pytha.pyox`: MediaType                   =
      new MediaType("model", "vnd.pytha.pyox", Compressible, NotBinary)
    lazy val `vnd.rosette.annotated-data-model`: MediaType =
      new MediaType("model", "vnd.rosette.annotated-data-model", Compressible, NotBinary)
    lazy val `vnd.sap.vds`: MediaType                      =
      new MediaType("model", "vnd.sap.vds", Compressible, NotBinary, List("vds"))
    lazy val `vnd.usdz+zip`: MediaType                     =
      new MediaType("model", "vnd.usdz+zip", Uncompressible, NotBinary, List("usdz"))
    lazy val `vnd.valve.source.compiled-map`: MediaType    =
      new MediaType("model", "vnd.valve.source.compiled-map", Compressible, NotBinary, List("bsp"))
    lazy val `vnd.vtu`: MediaType                          =
      new MediaType("model", "vnd.vtu", Compressible, NotBinary, List("vtu"))
    lazy val `vrml`: MediaType                             =
      new MediaType("model", "vrml", Uncompressible, NotBinary, List("wrl", "vrml"))
    lazy val `x3d+binary`: MediaType                       =
      new MediaType("model", "x3d+binary", Uncompressible, NotBinary, List("x3db", "x3dbz"))
    lazy val `x3d+fastinfoset`: MediaType                  =
      new MediaType("model", "x3d+fastinfoset", Compressible, NotBinary, List("x3db"))
    lazy val `x3d+vrml`: MediaType                         =
      new MediaType("model", "x3d+vrml", Uncompressible, NotBinary, List("x3dv", "x3dvz"))
    lazy val `x3d+xml`: MediaType                          =
      new MediaType("model", "x3d+xml", Compressible, NotBinary, List("x3d", "x3dz"))
    lazy val `x3d-vrml`: MediaType                         =
      new MediaType("model", "x3d-vrml", Compressible, NotBinary, List("x3dv"))
    lazy val all: List[MediaType]                          = List(
      `3mf`,
      `e57`,
      `gltf+json`,
      `gltf-binary`,
      `iges`,
      `mesh`,
      `mtl`,
      `obj`,
      `stl`,
      `vnd.collada+xml`,
      `vnd.dwf`,
      `vnd.flatland.3dml`,
      `vnd.gdl`,
      `vnd.gs-gdl`,
      `vnd.gs.gdl`,
      `vnd.gtw`,
      `vnd.moml+xml`,
      `vnd.mts`,
      `vnd.opengex`,
      `vnd.parasolid.transmit.binary`,
      `vnd.parasolid.transmit.text`,
      `vnd.pytha.pyox`,
      `vnd.rosette.annotated-data-model`,
      `vnd.sap.vds`,
      `vnd.usdz+zip`,
      `vnd.valve.source.compiled-map`,
      `vnd.vtu`,
      `vrml`,
      `x3d+binary`,
      `x3d+fastinfoset`,
      `x3d+vrml`,
      `x3d+xml`,
      `x3d-vrml`,
    )
  }
  object multipart                         {
    lazy val `alternative`: MediaType       =
      new MediaType("multipart", "alternative", Uncompressible, NotBinary)
    lazy val `appledouble`: MediaType       =
      new MediaType("multipart", "appledouble", Compressible, NotBinary)
    lazy val `byteranges`: MediaType        =
      new MediaType("multipart", "byteranges", Compressible, NotBinary)
    lazy val `digest`: MediaType            = new MediaType("multipart", "digest", Compressible, NotBinary)
    lazy val `encrypted`: MediaType         =
      new MediaType("multipart", "encrypted", Uncompressible, NotBinary)
    lazy val `form-data`: MediaType         =
      new MediaType("multipart", "form-data", Uncompressible, NotBinary)
    lazy val `header-set`: MediaType        =
      new MediaType("multipart", "header-set", Compressible, NotBinary)
    lazy val `mixed`: MediaType             = new MediaType("multipart", "mixed", Compressible, NotBinary)
    lazy val `multilingual`: MediaType      =
      new MediaType("multipart", "multilingual", Compressible, NotBinary)
    lazy val `parallel`: MediaType          = new MediaType("multipart", "parallel", Compressible, NotBinary)
    lazy val `related`: MediaType           = new MediaType("multipart", "related", Uncompressible, NotBinary)
    lazy val `report`: MediaType            = new MediaType("multipart", "report", Compressible, NotBinary)
    lazy val `signed`: MediaType            = new MediaType("multipart", "signed", Uncompressible, NotBinary)
    lazy val `vnd.bint.med-plus`: MediaType =
      new MediaType("multipart", "vnd.bint.med-plus", Compressible, NotBinary)
    lazy val `voice-message`: MediaType     =
      new MediaType("multipart", "voice-message", Compressible, NotBinary)
    lazy val `x-mixed-replace`: MediaType   =
      new MediaType("multipart", "x-mixed-replace", Compressible, NotBinary)
    lazy val all: List[MediaType]           = List(
      `alternative`,
      `appledouble`,
      `byteranges`,
      `digest`,
      `encrypted`,
      `form-data`,
      `header-set`,
      `mixed`,
      `multilingual`,
      `parallel`,
      `related`,
      `report`,
      `signed`,
      `vnd.bint.med-plus`,
      `voice-message`,
      `x-mixed-replace`,
    )
  }
  object text                              {
    lazy val `1d-interleaved-parityfec`: MediaType =
      new MediaType("text", "1d-interleaved-parityfec", Compressible, NotBinary)
    lazy val `cache-manifest`: MediaType           =
      new MediaType("text", "cache-manifest", Compressible, NotBinary, List("appcache", "manifest"))
    lazy val `calendar`: MediaType                 =
      new MediaType("text", "calendar", Compressible, NotBinary, List("ics", "ifb"))
    lazy val `calender`: MediaType                 = new MediaType("text", "calender", Compressible, NotBinary)
    lazy val `cmd`: MediaType                      = new MediaType("text", "cmd", Compressible, NotBinary)
    lazy val `coffeescript`: MediaType             =
      new MediaType("text", "coffeescript", Compressible, NotBinary, List("coffee", "litcoffee"))
    lazy val `cql`: MediaType                      = new MediaType("text", "cql", Compressible, NotBinary)
    lazy val `cql-expression`: MediaType           =
      new MediaType("text", "cql-expression", Compressible, NotBinary)
    lazy val `cql-identifier`: MediaType           =
      new MediaType("text", "cql-identifier", Compressible, NotBinary)
    lazy val `css`: MediaType                      = new MediaType("text", "css", Compressible, NotBinary, List("css"))
    lazy val `csv`: MediaType                      = new MediaType("text", "csv", Compressible, NotBinary, List("csv"))
    lazy val `csv-schema`: MediaType               = new MediaType("text", "csv-schema", Compressible, NotBinary)
    lazy val `directory`: MediaType                = new MediaType("text", "directory", Compressible, NotBinary)
    lazy val `dns`: MediaType                      = new MediaType("text", "dns", Compressible, NotBinary)
    lazy val `ecmascript`: MediaType               = new MediaType("text", "ecmascript", Compressible, NotBinary)
    lazy val `encaprtp`: MediaType                 = new MediaType("text", "encaprtp", Compressible, NotBinary)
    lazy val `enriched`: MediaType                 = new MediaType("text", "enriched", Compressible, NotBinary)
    lazy val `fhirpath`: MediaType                 = new MediaType("text", "fhirpath", Compressible, NotBinary)
    lazy val `flexfec`: MediaType                  = new MediaType("text", "flexfec", Compressible, NotBinary)
    lazy val `fwdred`: MediaType                   = new MediaType("text", "fwdred", Compressible, NotBinary)
    lazy val `gff3`: MediaType                     = new MediaType("text", "gff3", Compressible, NotBinary)
    lazy val `grammar-ref-list`: MediaType         =
      new MediaType("text", "grammar-ref-list", Compressible, NotBinary)
    lazy val `html`: MediaType                     =
      new MediaType("text", "html", Compressible, NotBinary, List("html", "htm", "shtml"))
    lazy val `jade`: MediaType                     =
      new MediaType("text", "jade", Compressible, NotBinary, List("jade"))
    lazy val `javascript`: MediaType               = new MediaType("text", "javascript", Compressible, NotBinary)
    lazy val `jcr-cnd`: MediaType                  = new MediaType("text", "jcr-cnd", Compressible, NotBinary)
    lazy val `jsx`: MediaType                      = new MediaType("text", "jsx", Compressible, NotBinary, List("jsx"))
    lazy val `less`: MediaType                     =
      new MediaType("text", "less", Compressible, NotBinary, List("less"))
    lazy val `markdown`: MediaType                 =
      new MediaType("text", "markdown", Compressible, NotBinary, List("markdown", "md"))
    lazy val `mathml`: MediaType                   =
      new MediaType("text", "mathml", Compressible, NotBinary, List("mml"))
    lazy val `mdx`: MediaType                      = new MediaType("text", "mdx", Compressible, NotBinary, List("mdx"))
    lazy val `mizar`: MediaType                    = new MediaType("text", "mizar", Compressible, NotBinary)
    lazy val `n3`: MediaType                       = new MediaType("text", "n3", Compressible, NotBinary, List("n3"))
    lazy val `parameters`: MediaType               = new MediaType("text", "parameters", Compressible, NotBinary)
    lazy val `parityfec`: MediaType                = new MediaType("text", "parityfec", Compressible, NotBinary)
    lazy val `plain`: MediaType                    = new MediaType(
      "text",
      "plain",
      Compressible,
      NotBinary,
      List("txt", "text", "conf", "def", "list", "log", "in", "ini"),
    )
    lazy val `provenance-notation`: MediaType      =
      new MediaType("text", "provenance-notation", Compressible, NotBinary)
    lazy val `prs.fallenstein.rst`: MediaType      =
      new MediaType("text", "prs.fallenstein.rst", Compressible, NotBinary)
    lazy val `prs.lines.tag`: MediaType            =
      new MediaType("text", "prs.lines.tag", Compressible, NotBinary, List("dsc"))
    lazy val `prs.prop.logic`: MediaType           =
      new MediaType("text", "prs.prop.logic", Compressible, NotBinary)
    lazy val `raptorfec`: MediaType                = new MediaType("text", "raptorfec", Compressible, NotBinary)
    lazy val `red`: MediaType                      = new MediaType("text", "red", Compressible, NotBinary)
    lazy val `rfc822-headers`: MediaType           =
      new MediaType("text", "rfc822-headers", Compressible, NotBinary)
    lazy val `richtext`: MediaType                 =
      new MediaType("text", "richtext", Compressible, NotBinary, List("rtx"))
    lazy val `rtf`: MediaType                      = new MediaType("text", "rtf", Compressible, NotBinary, List("rtf"))
    lazy val `rtp-enc-aescm128`: MediaType         =
      new MediaType("text", "rtp-enc-aescm128", Compressible, NotBinary)
    lazy val `rtploopback`: MediaType              =
      new MediaType("text", "rtploopback", Compressible, NotBinary)
    lazy val `rtx`: MediaType                      = new MediaType("text", "rtx", Compressible, NotBinary)
    lazy val `sgml`: MediaType                     =
      new MediaType("text", "sgml", Compressible, NotBinary, List("sgml", "sgm"))
    lazy val `shaclc`: MediaType                   = new MediaType("text", "shaclc", Compressible, NotBinary)
    lazy val `shex`: MediaType                     =
      new MediaType("text", "shex", Compressible, NotBinary, List("shex"))
    lazy val `slim`: MediaType                     =
      new MediaType("text", "slim", Compressible, NotBinary, List("slim", "slm"))
    lazy val `spdx`: MediaType                     =
      new MediaType("text", "spdx", Compressible, NotBinary, List("spdx"))
    lazy val `strings`: MediaType                  = new MediaType("text", "strings", Compressible, NotBinary)
    lazy val `stylus`: MediaType                   =
      new MediaType("text", "stylus", Compressible, NotBinary, List("stylus", "styl"))
    lazy val `t140`: MediaType                     = new MediaType("text", "t140", Compressible, NotBinary)
    lazy val `tab-separated-values`: MediaType     =
      new MediaType("text", "tab-separated-values", Compressible, NotBinary, List("tsv"))
    lazy val `troff`: MediaType                    = new MediaType(
      "text",
      "troff",
      Compressible,
      NotBinary,
      List("t", "tr", "roff", "man", "me", "ms"),
    )
    lazy val `turtle`: MediaType                   =
      new MediaType("text", "turtle", Compressible, NotBinary, List("ttl"))
    lazy val `ulpfec`: MediaType                   = new MediaType("text", "ulpfec", Compressible, NotBinary)
    lazy val `uri-list`: MediaType                 =
      new MediaType("text", "uri-list", Compressible, NotBinary, List("uri", "uris", "urls"))
    lazy val `vcard`: MediaType                    =
      new MediaType("text", "vcard", Compressible, NotBinary, List("vcard"))
    lazy val `vnd.a`: MediaType                    = new MediaType("text", "vnd.a", Compressible, NotBinary)
    lazy val `vnd.abc`: MediaType                  = new MediaType("text", "vnd.abc", Compressible, NotBinary)
    lazy val `vnd.ascii-art`: MediaType            =
      new MediaType("text", "vnd.ascii-art", Compressible, NotBinary)
    lazy val `vnd.curl`: MediaType                 =
      new MediaType("text", "vnd.curl", Compressible, NotBinary, List("curl"))
    lazy val `vnd.curl.dcurl`: MediaType           =
      new MediaType("text", "vnd.curl.dcurl", Compressible, NotBinary, List("dcurl"))
    lazy val `vnd.curl.mcurl`: MediaType           =
      new MediaType("text", "vnd.curl.mcurl", Compressible, NotBinary, List("mcurl"))
    lazy val `vnd.curl.scurl`: MediaType           =
      new MediaType("text", "vnd.curl.scurl", Compressible, NotBinary, List("scurl"))
    lazy val `vnd.debian.copyright`: MediaType     =
      new MediaType("text", "vnd.debian.copyright", Compressible, NotBinary)
    lazy val `vnd.dmclientscript`: MediaType       =
      new MediaType("text", "vnd.dmclientscript", Compressible, NotBinary)
    lazy val `vnd.dvb.subtitle`: MediaType         =
      new MediaType("text", "vnd.dvb.subtitle", Compressible, NotBinary, List("sub"))
    lazy val `vnd.esmertec.theme-descriptor`: MediaType    =
      new MediaType("text", "vnd.esmertec.theme-descriptor", Compressible, NotBinary)
    lazy val `vnd.ficlab.flt`: MediaType                   =
      new MediaType("text", "vnd.ficlab.flt", Compressible, NotBinary)
    lazy val `vnd.fly`: MediaType                          =
      new MediaType("text", "vnd.fly", Compressible, NotBinary, List("fly"))
    lazy val `vnd.fmi.flexstor`: MediaType                 =
      new MediaType("text", "vnd.fmi.flexstor", Compressible, NotBinary, List("flx"))
    lazy val `vnd.gml`: MediaType                          = new MediaType("text", "vnd.gml", Compressible, NotBinary)
    lazy val `vnd.graphviz`: MediaType                     =
      new MediaType("text", "vnd.graphviz", Compressible, NotBinary, List("gv"))
    lazy val `vnd.hans`: MediaType                         = new MediaType("text", "vnd.hans", Compressible, NotBinary)
    lazy val `vnd.hgl`: MediaType                          = new MediaType("text", "vnd.hgl", Compressible, NotBinary)
    lazy val `vnd.in3d.3dml`: MediaType                    =
      new MediaType("text", "vnd.in3d.3dml", Compressible, NotBinary, List("3dml"))
    lazy val `vnd.in3d.spot`: MediaType                    =
      new MediaType("text", "vnd.in3d.spot", Compressible, NotBinary, List("spot"))
    lazy val `vnd.iptc.newsml`: MediaType                  =
      new MediaType("text", "vnd.iptc.newsml", Compressible, NotBinary)
    lazy val `vnd.iptc.nitf`: MediaType                    =
      new MediaType("text", "vnd.iptc.nitf", Compressible, NotBinary)
    lazy val `vnd.latex-z`: MediaType                      =
      new MediaType("text", "vnd.latex-z", Compressible, NotBinary)
    lazy val `vnd.motorola.reflex`: MediaType              =
      new MediaType("text", "vnd.motorola.reflex", Compressible, NotBinary)
    lazy val `vnd.ms-mediapackage`: MediaType              =
      new MediaType("text", "vnd.ms-mediapackage", Compressible, NotBinary)
    lazy val `vnd.net2phone.commcenter.command`: MediaType =
      new MediaType("text", "vnd.net2phone.commcenter.command", Compressible, NotBinary)
    lazy val `vnd.radisys.msml-basic-layout`: MediaType    =
      new MediaType("text", "vnd.radisys.msml-basic-layout", Compressible, NotBinary)
    lazy val `vnd.senx.warpscript`: MediaType              =
      new MediaType("text", "vnd.senx.warpscript", Compressible, NotBinary)
    lazy val `vnd.si.uricatalogue`: MediaType              =
      new MediaType("text", "vnd.si.uricatalogue", Compressible, NotBinary)
    lazy val `vnd.sosi`: MediaType                         = new MediaType("text", "vnd.sosi", Compressible, NotBinary)
    lazy val `vnd.sun.j2me.app-descriptor`: MediaType      =
      new MediaType("text", "vnd.sun.j2me.app-descriptor", Compressible, NotBinary, List("jad"))
    lazy val `vnd.trolltech.linguist`: MediaType           =
      new MediaType("text", "vnd.trolltech.linguist", Compressible, NotBinary)
    lazy val `vnd.wap.si`: MediaType            = new MediaType("text", "vnd.wap.si", Compressible, NotBinary)
    lazy val `vnd.wap.sl`: MediaType            = new MediaType("text", "vnd.wap.sl", Compressible, NotBinary)
    lazy val `vnd.wap.wml`: MediaType           =
      new MediaType("text", "vnd.wap.wml", Compressible, NotBinary, List("wml"))
    lazy val `vnd.wap.wmlscript`: MediaType     =
      new MediaType("text", "vnd.wap.wmlscript", Compressible, NotBinary, List("wmls"))
    lazy val `vtt`: MediaType                   = new MediaType("text", "vtt", Compressible, NotBinary, List("vtt"))
    lazy val `x-asm`: MediaType                 =
      new MediaType("text", "x-asm", Compressible, NotBinary, List("s", "asm"))
    lazy val `x-c`: MediaType                   = new MediaType(
      "text",
      "x-c",
      Compressible,
      NotBinary,
      List("c", "cc", "cxx", "cpp", "h", "hh", "dic"),
    )
    lazy val `x-component`: MediaType           =
      new MediaType("text", "x-component", Compressible, NotBinary, List("htc"))
    lazy val `x-fortran`: MediaType             =
      new MediaType("text", "x-fortran", Compressible, NotBinary, List("f", "for", "f77", "f90"))
    lazy val `x-gwt-rpc`: MediaType             = new MediaType("text", "x-gwt-rpc", Compressible, NotBinary)
    lazy val `x-handlebars-template`: MediaType =
      new MediaType("text", "x-handlebars-template", Compressible, NotBinary, List("hbs"))
    lazy val `x-java-source`: MediaType         =
      new MediaType("text", "x-java-source", Compressible, NotBinary, List("java"))
    lazy val `x-jquery-tmpl`: MediaType         =
      new MediaType("text", "x-jquery-tmpl", Compressible, NotBinary)
    lazy val `x-lua`: MediaType                 =
      new MediaType("text", "x-lua", Compressible, NotBinary, List("lua"))
    lazy val `x-markdown`: MediaType            =
      new MediaType("text", "x-markdown", Compressible, NotBinary, List("mkd"))
    lazy val `x-nfo`: MediaType                 =
      new MediaType("text", "x-nfo", Compressible, NotBinary, List("nfo"))
    lazy val `x-opml`: MediaType                =
      new MediaType("text", "x-opml", Compressible, NotBinary, List("opml"))
    lazy val `x-org`: MediaType                 =
      new MediaType("text", "x-org", Compressible, NotBinary, List("org"))
    lazy val `x-pascal`: MediaType              =
      new MediaType("text", "x-pascal", Compressible, NotBinary, List("p", "pas"))
    lazy val `x-processing`: MediaType          =
      new MediaType("text", "x-processing", Compressible, NotBinary, List("pde"))
    lazy val `x-sass`: MediaType                =
      new MediaType("text", "x-sass", Compressible, NotBinary, List("sass"))
    lazy val `x-scss`: MediaType                =
      new MediaType("text", "x-scss", Compressible, NotBinary, List("scss"))
    lazy val `x-setext`: MediaType              =
      new MediaType("text", "x-setext", Compressible, NotBinary, List("etx"))
    lazy val `x-sfv`: MediaType                 =
      new MediaType("text", "x-sfv", Compressible, NotBinary, List("sfv"))
    lazy val `x-suse-ymp`: MediaType            =
      new MediaType("text", "x-suse-ymp", Compressible, NotBinary, List("ymp"))
    lazy val `x-uuencode`: MediaType            =
      new MediaType("text", "x-uuencode", Compressible, NotBinary, List("uu"))
    lazy val `x-vcalendar`: MediaType           =
      new MediaType("text", "x-vcalendar", Compressible, NotBinary, List("vcs"))
    lazy val `x-vcard`: MediaType               =
      new MediaType("text", "x-vcard", Compressible, NotBinary, List("vcf"))
    lazy val `xml`: MediaType                   = new MediaType("text", "xml", Compressible, NotBinary, List("xml"))
    lazy val `xml-external-parsed-entity`: MediaType =
      new MediaType("text", "xml-external-parsed-entity", Compressible, NotBinary)
    lazy val `yaml`: MediaType                       =
      new MediaType("text", "yaml", Compressible, NotBinary, List("yaml", "yml"))
    lazy val all: List[MediaType]                    = List(
      `1d-interleaved-parityfec`,
      `cache-manifest`,
      `calendar`,
      `calender`,
      `cmd`,
      `coffeescript`,
      `cql`,
      `cql-expression`,
      `cql-identifier`,
      `css`,
      `csv`,
      `csv-schema`,
      `directory`,
      `dns`,
      `ecmascript`,
      `encaprtp`,
      `enriched`,
      `fhirpath`,
      `flexfec`,
      `fwdred`,
      `gff3`,
      `grammar-ref-list`,
      `html`,
      `jade`,
      `javascript`,
      `jcr-cnd`,
      `jsx`,
      `less`,
      `markdown`,
      `mathml`,
      `mdx`,
      `mizar`,
      `n3`,
      `parameters`,
      `parityfec`,
      `plain`,
      `provenance-notation`,
      `prs.fallenstein.rst`,
      `prs.lines.tag`,
      `prs.prop.logic`,
      `raptorfec`,
      `red`,
      `rfc822-headers`,
      `richtext`,
      `rtf`,
      `rtp-enc-aescm128`,
      `rtploopback`,
      `rtx`,
      `sgml`,
      `shaclc`,
      `shex`,
      `slim`,
      `spdx`,
      `strings`,
      `stylus`,
      `t140`,
      `tab-separated-values`,
      `troff`,
      `turtle`,
      `ulpfec`,
      `uri-list`,
      `vcard`,
      `vnd.a`,
      `vnd.abc`,
      `vnd.ascii-art`,
      `vnd.curl`,
      `vnd.curl.dcurl`,
      `vnd.curl.mcurl`,
      `vnd.curl.scurl`,
      `vnd.debian.copyright`,
      `vnd.dmclientscript`,
      `vnd.dvb.subtitle`,
      `vnd.esmertec.theme-descriptor`,
      `vnd.ficlab.flt`,
      `vnd.fly`,
      `vnd.fmi.flexstor`,
      `vnd.gml`,
      `vnd.graphviz`,
      `vnd.hans`,
      `vnd.hgl`,
      `vnd.in3d.3dml`,
      `vnd.in3d.spot`,
      `vnd.iptc.newsml`,
      `vnd.iptc.nitf`,
      `vnd.latex-z`,
      `vnd.motorola.reflex`,
      `vnd.ms-mediapackage`,
      `vnd.net2phone.commcenter.command`,
      `vnd.radisys.msml-basic-layout`,
      `vnd.senx.warpscript`,
      `vnd.si.uricatalogue`,
      `vnd.sosi`,
      `vnd.sun.j2me.app-descriptor`,
      `vnd.trolltech.linguist`,
      `vnd.wap.si`,
      `vnd.wap.sl`,
      `vnd.wap.wml`,
      `vnd.wap.wmlscript`,
      `vtt`,
      `x-asm`,
      `x-c`,
      `x-component`,
      `x-fortran`,
      `x-gwt-rpc`,
      `x-handlebars-template`,
      `x-java-source`,
      `x-jquery-tmpl`,
      `x-lua`,
      `x-markdown`,
      `x-nfo`,
      `x-opml`,
      `x-org`,
      `x-pascal`,
      `x-processing`,
      `x-sass`,
      `x-scss`,
      `x-setext`,
      `x-sfv`,
      `x-suse-ymp`,
      `x-uuencode`,
      `x-vcalendar`,
      `x-vcard`,
      `xml`,
      `xml-external-parsed-entity`,
      `yaml`,
    )
  }
  object video                             {
    lazy val `1d-interleaved-parityfec`: MediaType =
      new MediaType("video", "1d-interleaved-parityfec", Compressible, Binary)
    lazy val `3gpp`: MediaType                     =
      new MediaType("video", "3gpp", Compressible, Binary, List("3gp", "3gpp"))
    lazy val `3gpp-tt`: MediaType                  = new MediaType("video", "3gpp-tt", Compressible, Binary)
    lazy val `3gpp2`: MediaType                    = new MediaType("video", "3gpp2", Compressible, Binary, List("3g2"))
    lazy val `av1`: MediaType                      = new MediaType("video", "av1", Compressible, Binary)
    lazy val `bmpeg`: MediaType                    = new MediaType("video", "bmpeg", Compressible, Binary)
    lazy val `bt656`: MediaType                    = new MediaType("video", "bt656", Compressible, Binary)
    lazy val `celb`: MediaType                     = new MediaType("video", "celb", Compressible, Binary)
    lazy val `dv`: MediaType                       = new MediaType("video", "dv", Compressible, Binary)
    lazy val `encaprtp`: MediaType                 = new MediaType("video", "encaprtp", Compressible, Binary)
    lazy val `ffv1`: MediaType                     = new MediaType("video", "ffv1", Compressible, Binary)
    lazy val `flexfec`: MediaType                  = new MediaType("video", "flexfec", Compressible, Binary)
    lazy val `h261`: MediaType                     = new MediaType("video", "h261", Compressible, Binary, List("h261"))
    lazy val `h263`: MediaType                     = new MediaType("video", "h263", Compressible, Binary, List("h263"))
    lazy val `h263-1998`: MediaType                = new MediaType("video", "h263-1998", Compressible, Binary)
    lazy val `h263-2000`: MediaType                = new MediaType("video", "h263-2000", Compressible, Binary)
    lazy val `h264`: MediaType                     = new MediaType("video", "h264", Compressible, Binary, List("h264"))
    lazy val `h264-rcdo`: MediaType                = new MediaType("video", "h264-rcdo", Compressible, Binary)
    lazy val `h264-svc`: MediaType                 = new MediaType("video", "h264-svc", Compressible, Binary)
    lazy val `h265`: MediaType                     = new MediaType("video", "h265", Compressible, Binary)
    lazy val `iso.segment`: MediaType              =
      new MediaType("video", "iso.segment", Compressible, Binary, List("m4s"))
    lazy val `jpeg`: MediaType                     = new MediaType("video", "jpeg", Compressible, Binary, List("jpgv"))
    lazy val `jpeg2000`: MediaType                 = new MediaType("video", "jpeg2000", Compressible, Binary)
    lazy val `jpm`: MediaType                      =
      new MediaType("video", "jpm", Compressible, Binary, List("jpm", "jpgm"))
    lazy val `mj2`: MediaType                      =
      new MediaType("video", "mj2", Compressible, Binary, List("mj2", "mjp2"))
    lazy val `mp1s`: MediaType                     = new MediaType("video", "mp1s", Compressible, Binary)
    lazy val `mp2p`: MediaType                     = new MediaType("video", "mp2p", Compressible, Binary)
    lazy val `mp2t`: MediaType                     = new MediaType("video", "mp2t", Compressible, Binary, List("ts"))
    lazy val `mp4`: MediaType                      =
      new MediaType("video", "mp4", Uncompressible, Binary, List("mp4", "mp4v", "mpg4"))
    lazy val `mp4v-es`: MediaType                  = new MediaType("video", "mp4v-es", Compressible, Binary)
    lazy val `mpeg`: MediaType                     = new MediaType(
      "video",
      "mpeg",
      Uncompressible,
      Binary,
      List("mpeg", "mpg", "mpe", "m1v", "m2v"),
    )
    lazy val `mpeg4-generic`: MediaType            =
      new MediaType("video", "mpeg4-generic", Compressible, Binary)
    lazy val `mpv`: MediaType                      = new MediaType("video", "mpv", Compressible, Binary)
    lazy val `nv`: MediaType                       = new MediaType("video", "nv", Compressible, Binary)
    lazy val `ogg`: MediaType                      = new MediaType("video", "ogg", Uncompressible, Binary, List("ogv"))
    lazy val `parityfec`: MediaType                = new MediaType("video", "parityfec", Compressible, Binary)
    lazy val `pointer`: MediaType                  = new MediaType("video", "pointer", Compressible, Binary)
    lazy val `quicktime`: MediaType                =
      new MediaType("video", "quicktime", Uncompressible, Binary, List("qt", "mov"))
    lazy val `raptorfec`: MediaType                = new MediaType("video", "raptorfec", Compressible, Binary)
    lazy val `raw`: MediaType                      = new MediaType("video", "raw", Compressible, Binary)
    lazy val `rtp-enc-aescm128`: MediaType         =
      new MediaType("video", "rtp-enc-aescm128", Compressible, Binary)
    lazy val `rtploopback`: MediaType              = new MediaType("video", "rtploopback", Compressible, Binary)
    lazy val `rtx`: MediaType                      = new MediaType("video", "rtx", Compressible, Binary)
    lazy val `scip`: MediaType                     = new MediaType("video", "scip", Compressible, Binary)
    lazy val `smpte291`: MediaType                 = new MediaType("video", "smpte291", Compressible, Binary)
    lazy val `smpte292m`: MediaType                = new MediaType("video", "smpte292m", Compressible, Binary)
    lazy val `ulpfec`: MediaType                   = new MediaType("video", "ulpfec", Compressible, Binary)
    lazy val `vc1`: MediaType                      = new MediaType("video", "vc1", Compressible, Binary)
    lazy val `vc2`: MediaType                      = new MediaType("video", "vc2", Compressible, Binary)
    lazy val `vnd.cctv`: MediaType                 = new MediaType("video", "vnd.cctv", Compressible, Binary)
    lazy val `vnd.dece.hd`: MediaType              =
      new MediaType("video", "vnd.dece.hd", Compressible, Binary, List("uvh", "uvvh"))
    lazy val `vnd.dece.mobile`: MediaType          =
      new MediaType("video", "vnd.dece.mobile", Compressible, Binary, List("uvm", "uvvm"))
    lazy val `vnd.dece.mp4`: MediaType             =
      new MediaType("video", "vnd.dece.mp4", Compressible, Binary)
    lazy val `vnd.dece.pd`: MediaType              =
      new MediaType("video", "vnd.dece.pd", Compressible, Binary, List("uvp", "uvvp"))
    lazy val `vnd.dece.sd`: MediaType              =
      new MediaType("video", "vnd.dece.sd", Compressible, Binary, List("uvs", "uvvs"))
    lazy val `vnd.dece.video`: MediaType           =
      new MediaType("video", "vnd.dece.video", Compressible, Binary, List("uvv", "uvvv"))
    lazy val `vnd.directv.mpeg`: MediaType         =
      new MediaType("video", "vnd.directv.mpeg", Compressible, Binary)
    lazy val `vnd.directv.mpeg-tts`: MediaType     =
      new MediaType("video", "vnd.directv.mpeg-tts", Compressible, Binary)
    lazy val `vnd.dlna.mpeg-tts`: MediaType        =
      new MediaType("video", "vnd.dlna.mpeg-tts", Compressible, Binary)
    lazy val `vnd.dvb.file`: MediaType             =
      new MediaType("video", "vnd.dvb.file", Compressible, Binary, List("dvb"))
    lazy val `vnd.fvt`: MediaType                  =
      new MediaType("video", "vnd.fvt", Compressible, Binary, List("fvt"))
    lazy val `vnd.hns.video`: MediaType            =
      new MediaType("video", "vnd.hns.video", Compressible, Binary)
    lazy val `vnd.iptvforum.1dparityfec-1010`: MediaType   =
      new MediaType("video", "vnd.iptvforum.1dparityfec-1010", Compressible, Binary)
    lazy val `vnd.iptvforum.1dparityfec-2005`: MediaType   =
      new MediaType("video", "vnd.iptvforum.1dparityfec-2005", Compressible, Binary)
    lazy val `vnd.iptvforum.2dparityfec-1010`: MediaType   =
      new MediaType("video", "vnd.iptvforum.2dparityfec-1010", Compressible, Binary)
    lazy val `vnd.iptvforum.2dparityfec-2005`: MediaType   =
      new MediaType("video", "vnd.iptvforum.2dparityfec-2005", Compressible, Binary)
    lazy val `vnd.iptvforum.ttsavc`: MediaType             =
      new MediaType("video", "vnd.iptvforum.ttsavc", Compressible, Binary)
    lazy val `vnd.iptvforum.ttsmpeg2`: MediaType           =
      new MediaType("video", "vnd.iptvforum.ttsmpeg2", Compressible, Binary)
    lazy val `vnd.motorola.video`: MediaType               =
      new MediaType("video", "vnd.motorola.video", Compressible, Binary)
    lazy val `vnd.motorola.videop`: MediaType              =
      new MediaType("video", "vnd.motorola.videop", Compressible, Binary)
    lazy val `vnd.mpegurl`: MediaType                      =
      new MediaType("video", "vnd.mpegurl", Compressible, Binary, List("mxu", "m4u"))
    lazy val `vnd.ms-playready.media.pyv`: MediaType       =
      new MediaType("video", "vnd.ms-playready.media.pyv", Compressible, Binary, List("pyv"))
    lazy val `vnd.nokia.interleaved-multimedia`: MediaType =
      new MediaType("video", "vnd.nokia.interleaved-multimedia", Compressible, Binary)
    lazy val `vnd.nokia.mp4vr`: MediaType                  =
      new MediaType("video", "vnd.nokia.mp4vr", Compressible, Binary)
    lazy val `vnd.nokia.videovoip`: MediaType              =
      new MediaType("video", "vnd.nokia.videovoip", Compressible, Binary)
    lazy val `vnd.objectvideo`: MediaType                  =
      new MediaType("video", "vnd.objectvideo", Compressible, Binary)
    lazy val `vnd.radgamettools.bink`: MediaType           =
      new MediaType("video", "vnd.radgamettools.bink", Compressible, Binary)
    lazy val `vnd.radgamettools.smacker`: MediaType        =
      new MediaType("video", "vnd.radgamettools.smacker", Compressible, Binary)
    lazy val `vnd.sealed.mpeg1`: MediaType                 =
      new MediaType("video", "vnd.sealed.mpeg1", Compressible, Binary)
    lazy val `vnd.sealed.mpeg4`: MediaType                 =
      new MediaType("video", "vnd.sealed.mpeg4", Compressible, Binary)
    lazy val `vnd.sealed.swf`: MediaType                   =
      new MediaType("video", "vnd.sealed.swf", Compressible, Binary)
    lazy val `vnd.sealedmedia.softseal.mov`: MediaType     =
      new MediaType("video", "vnd.sealedmedia.softseal.mov", Compressible, Binary)
    lazy val `vnd.uvvu.mp4`: MediaType                     =
      new MediaType("video", "vnd.uvvu.mp4", Compressible, Binary, List("uvu", "uvvu"))
    lazy val `vnd.vivo`: MediaType                         =
      new MediaType("video", "vnd.vivo", Compressible, Binary, List("viv"))
    lazy val `vnd.youtube.yt`: MediaType                   =
      new MediaType("video", "vnd.youtube.yt", Compressible, Binary)
    lazy val `vp8`: MediaType                              = new MediaType("video", "vp8", Compressible, Binary)
    lazy val `vp9`: MediaType                              = new MediaType("video", "vp9", Compressible, Binary)
    lazy val `webm`: MediaType                             =
      new MediaType("video", "webm", Uncompressible, Binary, List("webm"))
    lazy val `x-f4v`: MediaType       = new MediaType("video", "x-f4v", Compressible, Binary, List("f4v"))
    lazy val `x-fli`: MediaType       = new MediaType("video", "x-fli", Compressible, Binary, List("fli"))
    lazy val `x-flv`: MediaType       =
      new MediaType("video", "x-flv", Uncompressible, Binary, List("flv"))
    lazy val `x-m4v`: MediaType       = new MediaType("video", "x-m4v", Compressible, Binary, List("m4v"))
    lazy val `x-matroska`: MediaType  =
      new MediaType("video", "x-matroska", Uncompressible, Binary, List("mkv", "mk3d", "mks"))
    lazy val `x-mng`: MediaType       = new MediaType("video", "x-mng", Compressible, Binary, List("mng"))
    lazy val `x-ms-asf`: MediaType    =
      new MediaType("video", "x-ms-asf", Compressible, Binary, List("asf", "asx"))
    lazy val `x-ms-vob`: MediaType    =
      new MediaType("video", "x-ms-vob", Compressible, Binary, List("vob"))
    lazy val `x-ms-wm`: MediaType     =
      new MediaType("video", "x-ms-wm", Compressible, Binary, List("wm"))
    lazy val `x-ms-wmv`: MediaType    =
      new MediaType("video", "x-ms-wmv", Uncompressible, Binary, List("wmv"))
    lazy val `x-ms-wmx`: MediaType    =
      new MediaType("video", "x-ms-wmx", Compressible, Binary, List("wmx"))
    lazy val `x-ms-wvx`: MediaType    =
      new MediaType("video", "x-ms-wvx", Compressible, Binary, List("wvx"))
    lazy val `x-msvideo`: MediaType   =
      new MediaType("video", "x-msvideo", Compressible, Binary, List("avi"))
    lazy val `x-sgi-movie`: MediaType =
      new MediaType("video", "x-sgi-movie", Compressible, Binary, List("movie"))
    lazy val `x-smv`: MediaType       = new MediaType("video", "x-smv", Compressible, Binary, List("smv"))
    lazy val all: List[MediaType]     = List(
      `1d-interleaved-parityfec`,
      `3gpp`,
      `3gpp-tt`,
      `3gpp2`,
      `av1`,
      `bmpeg`,
      `bt656`,
      `celb`,
      `dv`,
      `encaprtp`,
      `ffv1`,
      `flexfec`,
      `h261`,
      `h263`,
      `h263-1998`,
      `h263-2000`,
      `h264`,
      `h264-rcdo`,
      `h264-svc`,
      `h265`,
      `iso.segment`,
      `jpeg`,
      `jpeg2000`,
      `jpm`,
      `mj2`,
      `mp1s`,
      `mp2p`,
      `mp2t`,
      `mp4`,
      `mp4v-es`,
      `mpeg`,
      `mpeg4-generic`,
      `mpv`,
      `nv`,
      `ogg`,
      `parityfec`,
      `pointer`,
      `quicktime`,
      `raptorfec`,
      `raw`,
      `rtp-enc-aescm128`,
      `rtploopback`,
      `rtx`,
      `scip`,
      `smpte291`,
      `smpte292m`,
      `ulpfec`,
      `vc1`,
      `vc2`,
      `vnd.cctv`,
      `vnd.dece.hd`,
      `vnd.dece.mobile`,
      `vnd.dece.mp4`,
      `vnd.dece.pd`,
      `vnd.dece.sd`,
      `vnd.dece.video`,
      `vnd.directv.mpeg`,
      `vnd.directv.mpeg-tts`,
      `vnd.dlna.mpeg-tts`,
      `vnd.dvb.file`,
      `vnd.fvt`,
      `vnd.hns.video`,
      `vnd.iptvforum.1dparityfec-1010`,
      `vnd.iptvforum.1dparityfec-2005`,
      `vnd.iptvforum.2dparityfec-1010`,
      `vnd.iptvforum.2dparityfec-2005`,
      `vnd.iptvforum.ttsavc`,
      `vnd.iptvforum.ttsmpeg2`,
      `vnd.motorola.video`,
      `vnd.motorola.videop`,
      `vnd.mpegurl`,
      `vnd.ms-playready.media.pyv`,
      `vnd.nokia.interleaved-multimedia`,
      `vnd.nokia.mp4vr`,
      `vnd.nokia.videovoip`,
      `vnd.objectvideo`,
      `vnd.radgamettools.bink`,
      `vnd.radgamettools.smacker`,
      `vnd.sealed.mpeg1`,
      `vnd.sealed.mpeg4`,
      `vnd.sealed.swf`,
      `vnd.sealedmedia.softseal.mov`,
      `vnd.uvvu.mp4`,
      `vnd.vivo`,
      `vnd.youtube.yt`,
      `vp8`,
      `vp9`,
      `webm`,
      `x-f4v`,
      `x-fli`,
      `x-flv`,
      `x-m4v`,
      `x-matroska`,
      `x-mng`,
      `x-ms-asf`,
      `x-ms-vob`,
      `x-ms-wm`,
      `x-ms-wmv`,
      `x-ms-wmx`,
      `x-ms-wvx`,
      `x-msvideo`,
      `x-sgi-movie`,
      `x-smv`,
    )
  }
  object x_conference                      {
    lazy val `x-cooltalk`: MediaType =
      new MediaType("x-conference", "x-cooltalk", Compressible, NotBinary, List("ice"))
    lazy val all: List[MediaType]    = List(`x-cooltalk`)
  }
  object x_shader                          {
    lazy val `x-fragment`: MediaType =
      new MediaType("x-shader", "x-fragment", Compressible, NotBinary)
    lazy val `x-vertex`: MediaType   = new MediaType("x-shader", "x-vertex", Compressible, NotBinary)
    lazy val all: List[MediaType]    = List(`x-fragment`, `x-vertex`)
  }
}
