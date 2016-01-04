/* license: CC0 */

function set_build_status (pkg_string)
{
  /* Find the element to put the status icon in. */
  var pkgIcon = document.getElementById("icon-"+ pkg_string);

  /* Don't bother when the icon doesn't exist. */
  if (pkgIcon != null)
  {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
      if (xhttp.readyState == 4 && xhttp.status == 200) {

          /* The API call returns JSON. Parse it, and change the icon's source. */
          var pkgInfo = JSON.parse(xhttp.responseText);
          pkgIcon.src = "../static/base/img/status-icons/"+ pkgInfo[0]["buildstatus"] + ".png";
      }
    }
    xhttp.open("GET", "http://hydra.gnu.org/api/latestbuilds?nr=1&project=gnu&jobset=master&job="+ pkg_string, true);
    xhttp.send();
  }
}

function show_hide(idThing)
{
  if(document.getElementById && document.createTextNode) {
    var thing = document.getElementById(idThing);
    /* Used to change the link text, depending on whether description is
       collapsed or expanded */
    var thingLink = thing.previousSibling.lastChild.firstChild;
    if (thing) {
      if (thing.style.display == "none") {
        var column = thing.parentNode;
        var pkg_icons = column.getElementsByTagName('img')
        for (var i=0; i < pkg_icons.length; i++) {
            set_build_status (pkg_icons[i].id.slice(5));
        }
        thing.style.display = "";
        thingLink.data = 'Collapse';
      } else {
        thing.style.display = "none";
        thingLink.data = 'Expand';
      }
    }
  }
}

/* Add controllers used for collapse/expansion of package descriptions */
function prep(idThing)
{
  var tdThing = document.getElementById(idThing).parentNode;
  if (tdThing) {
    var aThing = tdThing.firstChild.appendChild(document.createElement('a'));
    aThing.setAttribute('href', 'javascript:void(0)');
    aThing.setAttribute('title', 'show/hide package description');
    aThing.appendChild(document.createTextNode('Expand'));
    aThing.onclick=function(){show_hide(idThing);};
    /* aThing.onkeypress=function(){show_hide(idThing);}; */
  }
}

/* Take n element IDs, prepare them for javascript enhanced
   display and hide the IDs by default. */
function prep_pkg_descs()
{
  if(document.getElementById && document.createTextNode) {
    for(var i=0; i<arguments.length; i++) {
      prep(arguments[i])
      show_hide(arguments[i]);
    }
  }
}
