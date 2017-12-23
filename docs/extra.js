
// detect language by the filename
var file = location.pathname.split( "/" ).pop();

var lang;
if (file.endsWith("_ja.html")) {
    lang = "ja";
} else if (file.endsWith("_cn.html")) {
    lang = "cn";
} else {
    lang = "en";
}

// insert language-specific css
if (lang != "en") {
    var link = document.createElement("link");
    link.href = "../../../" + lang + ".css";
    link.type = "text/css";
    link.rel = "stylesheet";
    link.media = "screen,print";
    document.getElementsByTagName("head")[0].appendChild(link);
}