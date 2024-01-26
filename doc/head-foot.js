// google analytics stuff
var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-30539510-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

function lastmoddate() {
  update = new Date(document.lastModified);
  //update = document.lastModified;
  theMonth = update.getMonth() + 1;
  theDay = update.getDate();
  theYear = update.getFullYear();
  return  update //theDay + "/" + theMonth + "/" + theYear;
}

function footer() {
  document.writeln("<div id=\"dyn-footer\">")
  document.writeln("<hr class=\"slim\">")
  document.writeln("Software: <a href=\"http://www.michael-edwards.org\">Michael Edwards</a><br />")
  document.writeln("Documentation: <a href=\"http://www.michael-edwards.org\">Michael Edwards</a> & ")
  document.writeln("<a href=\"http://www.seanreed.ie\">Sean Reed</a><br /> ")
  document.writeln("Last update: " + lastmoddate());
  document.writeln("</div>");
}

// files in the doc directory will call this with argument "./" but those in
// subdirectories will need "../" 
function header(DocDir) {
    document.write("");
  document.write("<h1 id=\"tight\"><a href=\"" + DocDir + "index.html\" id=\"tight\">slippery chicken</a><\/h1>");
  document.write("<h6 id=\"tight\">specialised algorithmic composition software<\/h6>");
  document.write("");
  document.write("<!-- *********************************************************************** -->");
  document.write("<ul id=\"menu\">");
  document.write("<li>");
  document.write("<dl>");
  document.write("        <dt>about<\/dt>");
  document.write("        <dd><a href=\"" + DocDir + "overview.html\" title=\"detailed overview\">");
  document.write("          overview<\/a><\/dd>");
  document.write("        <dd><a href=\"" + DocDir + "events.html\" title=\"workshops, talks, etc.\">");
  document.write("          events<\/a><\/dd>");
  document.write("        <dd><a href=\"" + DocDir + "workshops.html\" title=\"workshops\">");
  document.write("          workshops<\/a><\/dd>");
  document.write("        <dd><a href=\"" + DocDir + "music.html\" title=\"music made with slippery chicken\">");
  document.write("          music<\/a><\/dd>");
  document.write("        <dd><a href=\"" + DocDir + "credits.html\"");
  document.write("          title=\"thanks to people who made this possible\">");
  document.write("          credits<\/a><\/dd>");
  document.write("        <dd><a href=\"" + DocDir + "contact.html\"");
  document.write("          title=\"email / forum / bugs etc.\">");
  document.write("          contact<\/a><\/dd>");
  document.write("<\/dl>");
  document.write("<\/li>");
  document.write("<!-- ********************************* -->");
  document.write("<li>");
  document.write("<dl>");
  document.write("  <dt>documentation<\/dt>");
  document.write("  <dd><a href=\"" + DocDir + "installation.html\" title=\"how to install\">");
  document.write("    installation<\/a><\/dd>");
  document.write("    <dd><a href=\"" + DocDir + "manual\/index.html\" title=\"user manual\">");
  document.write("      manual<\/a><\/dd>");
  document.write("    <dd><a href=\"" + DocDir + "robodoc.html\" title=\"source code documentation\">");
  document.write("      code doc<\/a><\/dd>");
  document.write("    <dd><a href=\"" + DocDir + "videos.html\" title=\"instructional videos\">");
      document.write("      videos<\/a><\/dd>");
  document.write("    <dd><a href=\"" + DocDir + "faq.html\" title=\"Frequently Asked Questions\">");
  document.write("      FAQ<\/a><\/dd>");
//  document.write("    <dd><a href=\"http:\/\/groups.google.com\/group\/slippery-chicken\"");
//  document.write("      title=\"user discussion and queries\">");
//  document.write("      forum<\/a><\/dd>");
  document.write("    <dd><a href=\"" + DocDir + "bugs.html\" title=\"how to report a bug\">");
  document.write("      bugs<\/a><\/dd>");
//  document.write("    <dd><a href=\"" + DocDir + "search.html\" title=\"search\">");
//  document.write("    search<\/a><\/dd>");
  document.write("    <\/dl>");
  document.write("  <\/li>");
  document.write("<!-- ********************************* -->");
  document.write("<li>");
  document.write("<dl>");
  document.write("        <dt>downloads<\/dt>");
  document.write("        <dd><a href=\"" + DocDir + "source.html\" title=\"software\">");
  document.write("          software<\/a><\/dd>");
  document.write("        <dd><a href=\"" + DocDir + "licence.html\" title=\"licence\">");
  document.write("          licence<\/a><\/dd>");
  document.write("        <dd><a href=\"" + DocDir + "papers.html\" title=\"journal and conference papers\">");
  document.write("          papers<\/a><\/dd>");
  document.write("        <dd><a href=\"" + DocDir + "downloads-other.html\" title=\"graphics, sound, etc.\">");
  document.write("          other files<\/a><\/dd>");
  document.write("<\/dl>");
  document.write("<\/li>");
  document.write("<\/ul>");
  document.write("<!-- *********************************************************************** -->");
  document.write("");
}

