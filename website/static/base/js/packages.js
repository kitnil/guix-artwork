/* license: CC0 */

function show_hide(idThing)
{
  if(document.getElementById && document.createTextNode) {
    var thing = document.getElementById(idThing);
    /* Used to change the link text, depending on whether description is
       collapsed or expanded */
    var thingLink = thing.previousSibling.lastChild.firstChild;
    if (thing) {
      if (thing.style.display == "none") {
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
