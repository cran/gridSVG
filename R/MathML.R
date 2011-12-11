
indentInc <- "  "

incind <- function(indent) {
    paste(indent, indentInc, sep="")
}

explicitMathVariant <- function(fontfamily, fontface) {
    currentFonts <- getSVGFonts()
    stackname <- fontStackFromFontFamily(fontfamily, currentFonts)
    switch(stackname,
           sans=switch(fontface,
                       plain="sans-serif",
                       bold="bold-sans-serif",
                       italic=,
                       oblique="sans-serif-italic",
                       bold.italic="sans-serif-bold-italic"),
           serif=switch(fontface,
                        plain="normal",
                        bold="bold",
                        italic=,
                        oblique="italic",
                        bold.italic="bold-italic"),
           mono="monospace",
           switch(fontface,
                  plain="sans-serif",
                  bold="bold-sans-serif",
                  italic=,
                  oblique="sans-serif-italic",
                  bold.italic="sans-serif-bold-italic"))
}

unicode <-
c("&#x0020;", "&#x0021;", "&#x2200;", "&#x0023;", "&#x2203;", 
"&#x0025;", "&#x0026;", "&#x220B;", "&#x0028;", "&#x0029;", "&#x2217;", 
"&#x002B;", "&#x002C;", "&#x2212;", "&#x002E;", "&#x002F;", "&#x0030;", 
"&#x0031;", "&#x0032;", "&#x0033;", "&#x0034;", "&#x0035;", "&#x0036;", 
"&#x0037;", "&#x0038;", "&#x0039;", "&#x003A;", "&#x003B;", "&#x003C;", 
"&#x003D;", "&#x003E;", "&#x003F;", "&#x2245;", "&#x0391;", "&#x0392;", 
"&#x03A7;", "&#x0394;", "&#x0395;", "&#x03A6;", "&#x0393;", "&#x0397;", 
"&#x0399;", "&#x03D1;", "&#x039A;", "&#x039B;", "&#x039C;", "&#x039D;", 
"&#x039F;", "&#x03A0;", "&#x0398;", "&#x03A1;", "&#x03A3;", "&#x03A4;", 
"&#x03A5;", "&#x03C2;", "&#x03A9;", "&#x039E;", "&#x03A8;", "&#x0396;", 
"&#x005B;", "&#x2234;", "&#x005D;", "&#x22A5;", "&#x005F;", "&#xF8E5;", 
"&#x03B1;", "&#x03B2;", "&#x03C7;", "&#x03B4;", "&#x03B5;", "&#x03C6;", 
"&#x03B3;", "&#x03B7;", "&#x03B9;", "&#x03D5;", "&#x03BA;", "&#x03BB;", 
"&#x03BC;", "&#x03BD;", "&#x03BF;", "&#x03C0;", "&#x03B8;", "&#x03C1;", 
"&#x03C3;", "&#x03C4;", "&#x03C5;", "&#x03D6;", "&#x03C9;", "&#x03BE;", 
"&#x03C8;", "&#x03B6;", "&#x007B;", "&#x007C;", "&#x007D;", "&#x223C;", 
"&#x20AC;", "&#x03D2;", "&#x2032;", "&#x2264;", "&#x2044;", "&#x221E;", 
"&#x0192;", "&#x2663;", "&#x2666;", "&#x2665;", "&#x2660;", "&#x2194;", 
"&#x2190;", "&#x2191;", "&#x2192;", "&#x2193;", "&#x00B0;", "&#x00B1;", 
"&#x2033;", "&#x2265;", "&#x00D7;", "&#x221D;", "&#x2202;", "&#x2022;", 
"&#x00F7;", "&#x2260;", "&#x2261;", "&#x2248;", "&#x2026;", "&#xF8E6;", 
"&#xF8E7;", "&#x21B5;", "&#x2135;", "&#x2111;", "&#x211C;", "&#x2118;", 
"&#x2297;", "&#x2295;", "&#x2205;", "&#x2229;", "&#x222A;", "&#x2283;", 
"&#x2287;", "&#x2284;", "&#x2282;", "&#x2286;", "&#x2208;", "&#x2209;", 
"&#x2220;", "&#x2207;", "&#xF6DA;", "&#xF6D9;", "&#xF6DB;", "&#x220F;", 
"&#x221A;", "&#x22C5;", "&#x00AC;", "&#x2227;", "&#x2228;", "&#x21D4;", 
"&#x21D0;", "&#x21D1;", "&#x21D2;", "&#x21D3;", "&#x25CA;", "&#x2329;", 
"&#xF8E8;", "&#xF8E9;", "&#xF8EA;", "&#x2211;", "&#xF8EB;", "&#xF8EC;", 
"&#xF8ED;", "&#xF8EE;", "&#xF8EF;", "&#xF8F0;", "&#xF8F1;", "&#xF8F2;", 
"&#xF8F3;", "&#xF8F4;", "&#x232A;", "&#x222B;", "&#x2320;", "&#xF8F5;", 
"&#x2321;", "&#xF8F6;", "&#xF8F7;", "&#xF8F8;", "&#xF8F9;", "&#xF8FA;", 
"&#xF8FB;", "&#xF8FC;", "&#xF8FD;", "&#xF8FE;")

# See ~/Research/Rstuff/SVG/PlotMath/greek.R
greek <-
structure(c("&#x03B1;", "&#x03B2;", "&#x03B3;", "&#x03B4;", "&#x03B5;", 
"&#x03B6;", "&#x03B7;", "&#x03B8;", "&#x03B9;", "&#x03BA;", "&#x03BB;", 
"&#x03BC;", "&#x03BD;", "&#x03BE;", "&#x03BF;", "&#x03C0;", "&#x03C1;", 
"&#x03C2;", "&#x03C3;", "&#x03C4;", "&#x03C5;", "&#x03D5;", "&#x03C7;", 
"&#x03C8;", "&#x03C9;", "&#x0391;", "&#x0392;", "&#x0393;", "&#x0394;", 
"&#x0395;", "&#x0396;", "&#x0397;", "&#x0398;", "&#x0399;", "&#x039A;", 
"&#x039B;", "&#x039C;", "&#x039D;", "&#x039E;", "&#x039F;", "&#x03A0;", 
"&#x03A1;", "&#x03A2;", "&#x03A3;", "&#x03A4;", "&#x03A5;", "&#x03A6;", 
"&#x03A7;", "&#x03A8;", "&#x03A9;"), .Names = c("alpha", "beta", 
"gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", 
"kappa", "lambda", "mu", "nu", "xi", "omicron", "pi", "rho", 
"", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega", 
"Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", 
"Theta", "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", 
"Pi", "Rho", "", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", 
"Omega"))

symbolNames <-
    c("..."="&#x2026;",
      cdots="&#x22EF;",
      ldots="&#x2026;",
      greek,
      theta1="&#x03D1;",
      vartheta="&#x03D1;",
      phi1="&#x03C6;",
      sigma1="&#x03C2;",
      varsigma="&#x03C2;",
      omega1="&#x03D6;",
      Upsilon1="&#x03D2;",
      aleph="&#x05D0;",
      infinity="&#x221E;",
      partialdiff="&#x2202;",
      nabla="&#x2207;",
      degree="&#x00B0;",
      minute="&#x2032;",
      second="&#x2033;")

# The general idea with each of these mml*() functions is to
# create a single MathML element.
# This means that, if the output is a collection of several
# elements, we wrap the whole collection in an <mrow>

mmlJuxta <- function(e, indent, fontfamily, fontface) {
    paste(indent, '<mrow>\n',
          do.call("paste",
                  c(lapply(e[-1], toMML, indent, fontfamily, fontface),
                    list(sep=""))),
          indent, '</mrow>\n',
          sep="")
}

mmlBinOp <- function(e, indent, fontfamily, fontface, op) {
    paste(indent, '<mrow>\n',
          toMML(e[[2]], indent, fontfamily, fontface),
          indent, "<mo>", op, "</mo>\n",
          toMML(e[[3]], indent, fontfamily, fontface),
          indent, '</mrow>\n',
          sep="")
}

mmlParen <- function(e, indent, fontfamily, fontface) {
    paste(indent, "<mfenced><mrow>\n",
          toMML(e[[2]], incind(indent), fontfamily, fontface),
          indent, "</mrow></mfenced>\n",
          sep="")
}

mmlBrace <- function(e, indent, fontfamily, fontface) {
    paste(indent, '<mfenced open="" close=""><mrow>\n',
          toMML(e[[2]], incind(indent), fontfamily, fontface),
          indent, "</mrow></mfenced>\n",
          sep="")
}

delimiters <- c(lfloor="&#x230A;",
           rfloor="&#x230B;",
           lceil="&#x2308;",
           rceil="&#x2309;")

convertDelim <- function(delim) {
    if (delim %in% names(delimiters))
        delimiters[delim]
    else
        delim
}

mmlGroup <- function(e, indent, fontfamily, fontface) {
    # e[[2]] and e[[4]] are the delimiters
    if (length(e) < 4)
        stop("Invalid plotmath group()")
    delim1 <- convertDelim(as.character(e[[2]]))
    delim2 <- convertDelim(as.character(e[[4]]))
    paste(indent, '<mfenced open="', delim1, '" close="', delim2, '"><mrow>\n',
          toMML(e[[3]], incind(indent), fontfamily, fontface),
          indent, "</mrow></mfenced>\n",
          sep="")    
}

mmlSup <- function(e, indent, fontfamily, fontface) {
    paste(indent, "<msup>\n",
          toMML(e[[2]], incind(indent), fontfamily, fontface),
          toMML(e[[3]], incind(indent), fontfamily, fontface),
          indent, "</msup>\n",
          sep="")
}

mmlSub <- function(e, indent, fontfamily, fontface) {
    paste(indent, "<msub>\n",
          toMML(e[[2]], incind(indent), fontfamily, fontface),
          toMML(e[[3]], incind(indent), fontfamily, fontface),
          indent, "</msub>\n",
          sep="")
}

mmlSqrt <- function(e, indent, fontfamily, fontface) {
    if (length(e) > 2) {
        paste(indent, "<mroot>\n",
              toMML(e[[2]], incind(indent), fontfamily, fontface),
              toMML(e[[3]], incind(indent), fontfamily, fontface),
              indent, "</mroot>\n",
              sep="")
    } else {
        paste(indent, "<msqrt>\n",
              toMML(e[[2]], incind(indent), fontfamily, fontface),
              indent, "</msqrt>\n",
              sep="")
    }
}

mmlFont <- function(e, indent, fontfamily, fontface) {
    toMML(e[[2]], indent, fontfamily, fontface)
}

mmlStyle <- function(e, indent, fontfamily, fontface, style) {
    displaystyle <- switch(style,
                           display="true",
                           "false")
    scriptlevel <- switch(style,
                          display=0,
                          text=0,
                          script=1,
                          scriptscript=2)
    paste(indent, '<mstyle displaystyle="', displaystyle,
          '" scriptlevel="', scriptlevel, '">\n',
          toMML(e[[2]], incind(indent), fontfamily, fontface),
          indent, '</mstyle>\n',
          sep="")
}

mmlSymbol <- function(e, indent, fontfamily, fontface) {
    paste(indent, '<mtext mathvariant="',
          explicitMathVariant(fontfamily, fontface),
          '">', unicode[as.integer(charToRaw(e[[2]])) - 31],
          indent, "</mtext>\n", sep="")    
}

mmlCSL <- function(e, indent, fontfamily, fontface) {
    paste(indent, '<mfenced open="" close="">\n',
          paste(sapply(e[-1], toMML, incind(indent), fontfamily, fontface),
                collapse=""),
          indent, "</mfenced>\n",
          sep="")    
}

mmlAccent <- function(e, indent, fontfamily, fontface, accent) {
    paste(indent,
          indent, '<mover accent="true" align="center">\n',
          indent, '<mrow>\n',
          toMML(e[[2]], incind(indent), fontfamily, fontface),
          indent, '</mrow>\n',
          indent, '<mo stretchy="false">', accent, '</mo>\n',
          indent, '</mover>\n',
          sep="")
}

mmlWideAccent <- function(e, indent, fontfamily, fontface, accent) {
    paste(indent, '<mover accent="true" align="center">\n',
          indent, '<mrow>\n',
          toMML(e[[2]], incind(indent), fontfamily, fontface),
          indent, '</mrow>\n',
          indent, '<mo stretchy="true">', accent, '</mo>\n',
          indent, '</mover>\n',
          sep="")
}

mmlUnderline <- function(e, indent, fontfamily, fontface) {
    # NOTE: <mstack> and <msline> are currently not supported
    #       by Mozilla-based browsers (2011-11-21)
    paste(indent, '<munder>\n',
          indent, '<mrow>\n',
          toMML(e[[2]], incind(indent), fontfamily, fontface),
          indent, '</mrow>\n',
          indent, '<mo stretchy="true">&#x00AF;</mo>',
          indent, '</munder>\n',
          sep="")
}

mmlSpace <- function(e, indent, fontfamily, fontface) {
    # binary space
    space <- paste(indent, 
                   '<mtext mathvariant="',
                   explicitMathVariant(fontfamily, fontface),
                   '">&#x00A0;</mtext>\n',
                   sep="")
    if (length(e) > 2) {
        content <- 
            paste(toMML(e[[2]], incind(indent), fontfamily, fontface),
                  space,
                  toMML(e[[3]], incind(indent), fontfamily, fontface),
                  sep="")
    } else {
        content <- 
            paste(space,
                  toMML(e[[2]], incind(indent), fontfamily, fontface),
                  sep="")
    }
    paste(indent, '<mrow>\n',
          content,
          indent, '</mrow>\n',
          sep="")
}

mmlPhantom <- function(e, indent, fontfamily, fontface) {
    paste(indent, '<mphantom>\n',
          toMML(e[[2]], incind(indent), fontfamily, fontface),
          indent, '</mphantom>\n',
          sep="")
}

mmlFrac <- function(e, indent, fontfamily, fontface, lwd="medium") {
    paste(indent, '<mfrac linethickness="', lwd, '">\n',
          toMML(e[[2]], incind(indent), fontfamily, fontface),
          toMML(e[[3]], incind(indent), fontfamily, fontface),
          indent, '</mfrac>\n',
          sep="")
}

mmlBigOp <- function(e, indent, fontfamily, fontface, op=NULL) {
    # Either specify op special character or format the first
    # element of the expression
    if (is.null(op))
        opText <- paste(indent, '<mrow>\n',
                        toMML(e[[1]], incind(indent), fontfamily, fontface),
                        indent, 
                        '<mtext mathvariant="',
                        explicitMathVariant(fontfamily, fontface),
                        '">&#x00A0;</mtext>\n',
                        indent, '</mrow>\n',
                        sep="")
    else
        opText <- paste(indent, '<mo>', op, '</mo>\n', sep="")
    if (length(e) < 3) {
        paste(indent, '<mrow>\n',
              opText,
              toMML(e[[2]], incind(indent), fontfamily, fontface),
              indent, '</mrow>\n',
              sep="")
    } else if (length(e) < 4) {
        paste(indent, '<mrow>\n',
              indent, '<munder>\n',
              opText,
              toMML(e[[3]], incind(indent), fontfamily, fontface),
              indent, '</munder>\n',
              toMML(e[[2]], incind(indent), fontfamily, fontface),
              indent, '</mrow>\n',
              sep="")
    } else {
        paste(indent, '<mrow>\n',
              indent, '<munderover>\n',
              opText,
              toMML(e[[3]], incind(indent), fontfamily, fontface),
              toMML(e[[4]], incind(indent), fontfamily, fontface),
              indent, '</munderover>\n',
              toMML(e[[2]], incind(indent), fontfamily, fontface),
              indent, '</mrow>\n',
              sep="")
    }
}

mmlFun <- function(e, indent, fontfamily, fontface, op) {
    paste(indent, '<mrow>\n',
          indent,
          '<mtext mathvariant="',
          explicitMathVariant(fontfamily, fontface),
          '">', e[[1]], "</mtext>\n",
          indent, '<mfenced>\n',
          toMML(e[[2]], incind(indent), fontfamily, fontface),
          indent, '</mfenced>\n',
          indent, '</mrow>\n',
          sep="")
}

toMML <- function(x, indent, ...) {
    UseMethod("toMML")
}

toMML.numeric <- function(x, indent, ...) {
    paste(indent, "<mn>", as.character(x), "</mn>\n", sep="")
}

toMML.character <- function(x, indent, fontfamily, fontface, ...) {
    paste(indent,
          '<mtext mathvariant="',
          explicitMathVariant(fontfamily, fontface),
          '">', x, "</mtext>\n", sep="")
}

toMML.name <- function(x, indent, fontfamily, fontface, ...) {
    # Convert special names
    if (as.character(x) %in% names(symbolNames))
        x <- symbolNames[as.character(x)]
    # R does NOT automatically italicize symbols
    paste(indent,
          '<mtext mathvariant="',
          explicitMathVariant(fontfamily, fontface),
          '">', x, "</mtext>\n", sep="")
}

# A "language" object may have class "call" or "(" or "{"
"toMML.(" <- function(x, ...) {
    toMML.call(x, ...)
}

"toMML.{" <- function(x, ...) {
    toMML.call(x, ...)
}

funCallToMML <- function(x, indent, fontfamily, fontface) {
    funName <- as.character(x[[1]])
    switch(funName,
           "+"=,
           "/"=mmlBinOp(x, indent, fontfamily, fontface, funName),
           "-"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2212;"),
           "*"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2062;"),
           "%+-%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x00B1;"),
           "%/%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x00F7;"),
           "%*%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x00D7;"),
           "%.%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x22C5;"),
           "["=mmlSub(x, indent, fontfamily, fontface),
           "^"=mmlSup(x, indent, fontfamily, fontface),
           "paste"=mmlJuxta(x, indent, fontfamily, fontface),
           "sqrt"=mmlSqrt(x, indent, fontfamily, fontface),
           "("=mmlParen(x, indent, fontfamily, fontface),
           "{"=mmlBrace(x, indent, fontfamily, fontface),
           "=="=mmlBinOp(x, indent, fontfamily, fontface, "="),
           "!="=mmlBinOp(x, indent, fontfamily, fontface, "&#x2260;"),
           "<"=mmlBinOp(x, indent, fontfamily, fontface, "&lt;"),
           "<="=mmlBinOp(x, indent, fontfamily, fontface, "&#x2264;"),
           ">"=mmlBinOp(x, indent, fontfamily, fontface, "&gt;"),
           ">="=mmlBinOp(x, indent, fontfamily, fontface, "&#x2265;"),
           "%~~%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2248;"),
           "%=~%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2245;"),
           "%==%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2261;"),
           "%prop%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x221D;"),
           "plain"=mmlFont(x, indent, fontfamily, "plain"),
           "bold"=mmlFont(x, indent, fontfamily, "bold"),
           "italic"=mmlFont(x, indent, fontfamily, "italic"),
           "bolditalic"=mmlFont(x, indent, fontfamily, "bold.italic"),
           "symbol"=mmlSymbol(x, indent, fontfamily, fontface),
           "list"=mmlCSL(x, indent, fontfamily, fontface),
           "%subset%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2282;"),
           "%subseteq%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2286;"),
           "%notsubset%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2284;"),
           "%supset%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2283;"),
           "%supseteq%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2287;"),
           "%notsupset%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2285;"),
           "%in%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2208;"),
           "%notin%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2209;"),
           "hat"=mmlAccent(x, indent, fontfamily, fontface, "&#x005E;"),
           "tilde"=mmlAccent(x, indent, fontfamily, fontface, "&#x007E;"),
           "dot"=mmlAccent(x, indent, fontfamily, fontface, "&#x02D9;"),
           "ring"=mmlAccent(x, indent, fontfamily, fontface, "&#x02DA;"),
           # Used "macron"
           "bar"=mmlAccent(x, indent, fontfamily, fontface, "&#x00AF;"),
           # FIXME:  these are just normal accents positioned as limits
           "widehat"=mmlWideAccent(x, indent, fontfamily, fontface,
                                   "&#x005E;"),
           "widetilde"=mmlWideAccent(x, indent, fontfamily, fontface,
                                     "&#x007E;"),
           "%<->%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2194;"),
           "%->%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2192;"),
           "%<-%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2190;"),
           "%up%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2191;"),
           "%down%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x2193;"),
           "%<=>%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x21D4;"),
           "%=>%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x21D2;"),
           "%<=%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x21D0;"),
           "%dblup%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x21D1;"),
           "%dbldown%"=mmlBinOp(x, indent, fontfamily, fontface, "&#x21D3;"),
           "displaystyle"=mmlStyle(x, indent, fontfamily, fontface, "display"),
           "textstyle"=mmlStyle(x, indent, fontfamily, fontface, "text"),
           "scriptstyle"=mmlStyle(x, indent, fontfamily, fontface, "script"),
           "scriptscriptstyle"=mmlStyle(x, indent, fontfamily, fontface,
                                        "scriptscript"),
           "underline"=mmlUnderline(x, indent, fontfamily, fontface),
           "~"=mmlSpace(x, indent, fontfamily, fontface),
           "phantom"=mmlPhantom(x, indent, fontfamily, fontface),
           "over"=,
           "frac"=mmlFrac(x, indent, fontfamily, fontface),
           "atop"=mmlFrac(x, indent, fontfamily, fontface, lwd="0em"),
           "sum"=mmlBigOp(x, indent, fontfamily, fontface, "&#x2211;"),
           "prod"=mmlBigOp(x, indent, fontfamily, fontface, "&#x220F;"),
           "integral"=mmlBigOp(x, indent, fontfamily, fontface, "&#x222B;"),
           "union"=mmlBigOp(x, indent, fontfamily, fontface, "&#x22C3;"),
           "intersect"=mmlBigOp(x, indent, fontfamily, fontface, "&#x22C2;"),
           "prod"=mmlBigOp(x, indent, fontfamily, fontface, "&#x220F;"),
           "lim"=mmlBigOp(x, indent, fontfamily, fontface),
           "min"=mmlBigOp(x, indent, fontfamily, fontface),
           "inf"=mmlBigOp(x, indent, fontfamily, fontface),
           "sup"=mmlBigOp(x, indent, fontfamily, fontface),
           "group"=mmlGroup(x, indent, fontfamily, fontface),
           "bgroup"=mmlGroup(x, indent, fontfamily, fontface),
           mmlFun(x, indent, fontfamily, fontface))
}

# Table of Unicode math ops at
# http://www.w3.org/TR/MathML2/022.html
toMML.call <- function(x, indent, fontfamily, fontface, ...) {
    if (is.name(x[[1]])) {
        funCallToMML(x, indent, fontfamily, fontface)
    } else {
        # The function "name" may itself be a call
        paste(indent, '<mrow>\n',
              toMML(x[[1]], incind(indent), fontfamily, fontface),
              indent, '<mfenced>\n',
              toMML(x[[2]], incind(indent), fontfamily, fontface),
              indent, '</mfenced>\n',
              indent, '</mrow>\n',
              sep="")
    }
}

# fontfamily is used to set explicit 'mathvariant' when it is not
# implicit in the formula element
expr2mml <- function(e, fontfamily, fontface) {
    bits <- lapply(e, toMML, indent="    ",
                   fontfamily=fontfamily, fontface=fontface)
    # Use display="inline" to have some hope of positioning the
    # output somewhere near the right location
    paste('<math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">\n',
          paste(sapply(bits,
                       function(x) paste('  <mrow>\n', x, '  </mrow>\n',
                                         sep="")),
                collapse="\n"),
          '</math>\n', sep="")
}

