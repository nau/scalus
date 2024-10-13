let observer=null;const attrsToCopy=["data-githubContributorsUrl","data-githubContributorsFilename","data-pathToRoot","data-rawLocation","data-dynamicSideMenu"];function savePageState(e){const t={};for(const n of attrsToCopy)t[n]=e.documentElement.getAttribute(n);return{mainDiv:e.querySelector("#main")?.innerHTML,leftColumn:dynamicSideMenu?null:e.querySelector("#leftColumn").innerHTML,title:e.title,attrs:t}}function loadPageState(e,t){e.title=t.title,e.querySelector("#main").innerHTML=t.mainDiv,dynamicSideMenu||(e.querySelector("#leftColumn").innerHTML=t.leftColumn);for(const n of attrsToCopy)e.documentElement.setAttribute(n,t.attrs[n])}const attachedElements=new WeakSet;function attachAllListeners(){observer&&observer.disconnect();var e=document.querySelectorAll(".ni.n0.expanded").length>0,t=document.querySelector(".ni.n0");if(!e&&null!=t){t.querySelector("a");t.classList.add("expanded");var n=t.querySelector("button.ar");null!=n&&n.classList.add("expanded")}var o=sessionStorage.getItem("scroll_value");o&&document.querySelector(".side-menu").scrollTo(0,o);const a=window.location.hash,l=[...document.querySelectorAll("#content section[id]")].find((e=>a===`#${e.id}`));l&&document.querySelector("#main").scrollTo(0,l.offsetTop-100);var r=document.getElementsByClassName("documentableElement");if(r)for(i=0;i<r.length;i++){var s=r[i].querySelector(".documentableElement-expander");null!==r[i].querySelector(".show-content")&&null!==s&&(s.onclick=function(e){$(e.target).is("a")||!0===e.fromSnippet||(this.parentElement.classList.toggle("expand"),this.children[0].classList.toggle("expanded"),this.querySelector(".show-content").classList.toggle("expand"))})}document.querySelectorAll(".documentableElement .signature").forEach((e=>{const t=e.querySelector(".signature-short"),n=e.querySelector(".signature-long"),o=document.createElement("span"),a=document.createTextNode("...");o.appendChild(a),o.classList.add("extender"),t&&n&&e.children[1].hasChildNodes()&&e.children[0].append(o)}));[...document.getElementsByClassName("documentableList")].forEach((e=>{e.children[0].addEventListener("click",(()=>{e.classList.toggle("expand"),e.children[0].children[0].classList.toggle("expand")}))}));var d=document.getElementsByClassName("tab");if(d)for(i=0;i<d.length;i++)$(d[i].children[0].children[0]).is("button")&&(d[i].children[0].onclick=function(e){this.classList.toggle("expand"),this.children[0].classList.toggle("expand"),this.parentElement.classList.toggle("expand"),this.parentElement.parentElement.classList.toggle("expand")});[...document.querySelectorAll(".documentableBrief")].forEach((e=>{e.addEventListener("click",(()=>{e.parentElement.parentElement.parentElement.parentElement.classList.add("expand"),e.parentElement.parentElement.parentElement.previousElementSibling.children[0].classList.add("expanded")}))})),document.querySelectorAll("a").forEach((e=>{const t=e.href;if(""===t)return;const n=new URL(t);attachedElements.has(e)||(attachedElements.add(e),e.addEventListener("click",(e=>{n.href.replace(/#.*/,"")!==window.location.href.replace(/#.*/,"")&&n.origin===window.location.origin&&(e.metaKey||e.ctrlKey||e.shiftKey||e.altKey||0!==e.button||(e.preventDefault(),e.stopPropagation(),$.get(t,(function(e){const n=getRawLoc();null===window.history.state&&window.history.replaceState(savePageState(document),"");const o=savePageState((new DOMParser).parseFromString(e,"text/html"));window.history.pushState(o,"",t),loadPageState(document,o);const a=getRawLoc();dynamicSideMenu&&updateMenu(n,a),window.dispatchEvent(new Event(DYNAMIC_PAGE_LOAD)),document.querySelector("#main").scrollTo({top:0,left:0,behavior:"instant"})}))))})))})),document.querySelectorAll(".ar").forEach((e=>{attachedElements.has(e)||(attachedElements.add(e),e.addEventListener("click",(t=>{t.stopPropagation(),e.parentElement.parentElement.classList.toggle("expanded"),e.classList.toggle("expanded")})))})),document.querySelectorAll(".documentableList .ar").forEach((e=>{e.addEventListener("click",(()=>{e.parentElement.parentElement.classList.toggle("expand"),e.classList.toggle("expand")}))})),document.querySelectorAll(".nh").forEach((e=>{attachedElements.has(e)||(attachedElements.add(e),e.addEventListener("click",(()=>{e.lastChild.href.replace("#","")===window.location.href.replace("#","")?(e.parentElement.classList.toggle("expanded"),e.firstChild.classList.toggle("expanded")):e.lastChild.click()})))}));const c=e=>{"Show all"==e.textContent?e.textContent="Collapse":e.textContent="Show all"};document.querySelectorAll(".supertypes").forEach((e=>e.lastElementChild.addEventListener("click",(()=>{e.classList.toggle("collapsed"),c(e.lastElementChild)})))),document.querySelectorAll(".subtypes").forEach((e=>e.lastElementChild.addEventListener("click",(()=>{e.classList.toggle("collapsed"),c(e.lastElementChild)})))),document.querySelectorAll(".ni").forEach((e=>e.addEventListener("mouseenter",(t=>{sessionStorage.setItem("scroll_value",e.offsetTop-window.innerHeight/2)}))));const u=e=>e.target.getAttribute("id"),m=e=>document.querySelector(`#toc li a[href="#${e}"]`),h=[...document.querySelectorAll("#toc li a")].reduce(((e,t)=>(t.hash.length&&e.push(t.hash),e)),[]),g=()=>{h.forEach((e=>{const t=document.querySelector(`#toc li a[href="${e}"]`);t.parentElement?.classList?.contains("active")&&t.parentElement.classList.remove("active")}))};if((observer=new IntersectionObserver((e=>{const t=e[0],n=e[e.length-1],o=window.location.hash,a=document.querySelector(`#toc li a[href="${o}"]`);if(a&&(g(),a.parentElement?.classList.toggle("active")),e.length>3){g();const e=u(t);m(e).parentElement.classList.toggle("active")}if(n.isIntersecting){history.replaceState(history.state,"",window.location.pathname+window.location.search),g();const e=u(n);m(e).parentElement.classList.toggle("active")}}),{rootMargin:"-10% 0px -50%"}),document.querySelectorAll("#content section[id]").forEach((e=>{observer.observe(e)})),location.hash)&&"container"!=location.hash.substring(1)){var p=document.getElementById(location.hash.substring(1));if(p){p.classList.toggle("expand"),p.classList.toggle("expanded");const e=p.querySelector(".icon-button");e.classList.toggle("expand"),e.classList.toggle("expanded")}}document.querySelectorAll("pre code").forEach((e=>{hljs.highlightBlock(e)})),document.body.addEventListener("keydown",(e=>{if("f"==e.key){const t=e.target.tagName;if("INPUT"!=t&&"TEXTAREA"!=t){const e=findRef(".documentableFilter input.filterableInput");null!=e&&setTimeout((()=>e.focus()),1)}}}))}const DYNAMIC_PAGE_LOAD="dynamicPageLoad";window.addEventListener(DYNAMIC_PAGE_LOAD,(()=>{attachAllListeners()})),window.addEventListener(DYNAMIC_PAGE_LOAD,(()=>{if(sessionStorage.getItem("sideMenuOpen"))document.querySelector("#leftColumn").classList.contains("show")&&document.querySelector("#content").classList.add("sidebar-shown"),sessionStorage.removeItem("sideMenuOpen");else{const e=document.querySelector(".show");e&&e.classList.remove("show");const t=document.querySelector(".menu-shown");t&&t.classList.remove("menu-shown");const n=document.querySelector(".sidebar-shown");n&&n.classList.remove("sidebar-shown")}}));let dynamicSideMenu=!1;function updatePath(e,t,n=!0){if(e.classList.contains("side-menu"))return;const o=e.firstElementChild,a=o.firstElementChild;t?(e.classList.remove("expanded"),o.classList.remove("h100","selected","expanded","cs"),a&&a.classList.remove("expanded")):(e.classList.add("expanded"),o.classList.add("h100","expanded","cs"),a&&a.classList.add("expanded"),n&&o.classList.add("selected")),updatePath(e.parentElement,t,!1)}let updateMenu=null;function getRawLoc(){return document.documentElement.getAttribute("data-rawLocation")?.split("/")?.filter((e=>""!==e))}function render(e,{cls:t=null,id:n=null,href:o=null,loc:a=null}={},l=[]){const r=document.createElement(e);return t&&t.split(" ").filter((e=>""!==e)).forEach((e=>r.classList.add(e))),n&&(r.id=n),o&&(r.href=o),a&&r.setAttribute("data-loc",a),l.filter((e=>null!==e)).forEach((e=>r.appendChild("string"==typeof e?document.createTextNode(e):e))),r}function renderDynamicSideMenu(){const e=document.documentElement.getAttribute("data-pathToRoot"),t=e+"dynamicSideMenu.json",n=getRawLoc(),o=window.location.pathname.split("/").slice(0,-1-e.split("/").filter((e=>""!=e)).length);function a(e){return`${o}/${e.join("/")}.html`}fetch(t).then((e=>e.json())).then((e=>{function t(e,n,o,l){const r=e.name,s=l&&"package"===e.kind&&r.startsWith(o+".")?r.substring(o.length+1):r,i=""==o?s:o+"."+s,d=e.children.map((e=>t(e,n+1,i,l))),c=render("span",{cls:"nh "+(l?"":"de")},[d.length?render("button",{cls:"ar icon-button"}):null,render("a",{href:a(e.location)},[e.kind&&render("span",{cls:`micon ${e.kind.slice(0,2)}`}),render("span",{},[s])])]);e.location.join("/");return render("div",{cls:`ni n${n}`,loc:e.location.join("/")},[c,...d])}const o=render("div",{cls:"switcher-container"},[e.docs&&render("a",{id:"docs-nav-button",cls:"switcher h100",href:a(e.docs.location)},["Docs"]),e.api&&render("a",{id:"api-nav-button",cls:"switcher h100",href:a(e.api.location)},["API"])]),l=e.docs&&render("nav",{cls:"side-menu",id:"docs-nav"},e.docs.children.map((e=>t(e,0,"",!1)))),r=e.api&&render("nav",{cls:"side-menu",id:"api-nav"},e.api.children.map((e=>t(e,0,"",!0))));document.getElementById("leftColumn").appendChild(o),l&&document.getElementById("leftColumn").appendChild(l),r&&document.getElementById("leftColumn").appendChild(r),updateMenu=(t,n)=>{if(t){const e=document.querySelector(`[data-loc="${t.join("/")}"]`);e&&updatePath(e,!0)}l&&r&&(n[0]&&n[0]==e.api.location[0]?(l.hidden=!0,r.hidden=!1):(l.hidden=!1,r.hidden=!0));const o=document.querySelector(`[data-loc="${n.join("/")}"]`);o&&updatePath(o,!1)},updateMenu(null,n),window.dispatchEvent(new Event(DYNAMIC_PAGE_LOAD))}))}window.addEventListener("DOMContentLoaded",(()=>{hljs.registerLanguage("scala",highlightDotty),hljs.registerAliases(["dotty","scala3"],"scala"),dynamicSideMenu="true"===document.documentElement.getAttribute("data-dynamicSideMenu"),dynamicSideMenu?renderDynamicSideMenu():window.dispatchEvent(new Event(DYNAMIC_PAGE_LOAD))}));const elements=document.querySelectorAll(".documentableElement"),sideMenuToggler=document.getElementById("mobile-sidebar-toggle");var zoom,transform;function showGraph(){if(document.getElementById("inheritance-diagram").classList.add("shown"),0==$("svg#graph").children().length){var e=document.querySelector("#dot");if(e){var t=d3.select("#graph"),n=t.append("defs").append("radialGradient").attr("id","Gradient");n.append("stop").attr("stop-color","var(--yellow9)").attr("offset","30%"),n.append("stop").attr("stop-color","var(--background-default)").attr("offset","100%");var o=t.append("g");zoom=d3.zoom().on("zoom",(function({transform:e}){o.attr("transform",e)})),t.call(zoom);var a=new dagreD3.render,l=graphlibDot.read(e.text);l.graph().rankDir="BT",l.nodes().forEach((function(e){l.setNode(e,{labelType:"html",label:l.node(e).label,class:l.node(e).class,id:l.node(e).id,rx:"4px",ry:"4px"})})),l.setNode("node0Cluster",{style:"fill: url(#Gradient);",id:"node0Cluster"}),l.setParent("node0","node0Cluster"),l.edges().forEach((function(e){l.setEdge(e,{arrowhead:"hollowPoint"})})),a.arrows().hollowPoint=function(e,t,n,o){var a=e.append("marker").attr("id",t).attr("viewBox","0 0 10 10").attr("refX",9).attr("refY",5).attr("markerUnits","strokeWidth").attr("markerWidth",12).attr("markerHeight",12).attr("orient","auto").append("path").attr("d","M 0 0 L 10 5 L 0 10 z").style("stroke-width",1).style("stroke-dasharray","1,0").style("fill","var(--grey12)").style("stroke","var(--grey12)");dagreD3.util.applyStyle(a,n[o+"Style"])},a(o,l);var r=t.node().getBBox(),s=t.node().parentElement,i=s.clientWidth||s.parentNode.clientWidth,d=s.clientHeight||s.parentNode.clientHeight,c=r.width,u=r.height,m=r.x+c/2,h=r.y+u/2;if(0==c||0==u)return;var g=.99*Math.min(i/c,d/u),p=[i/2-g*m,d/2-g*h];transform=d3.zoomIdentity.translate(p[0],p[1]).scale(g),t.call(zoom.transform,transform);var f=d3.select("g#node0")._groups[0][0],E=f.children[0],y=d3.select("g#node0Cluster")._groups[0][0],L=y.children[0];y.setAttribute("transform",f.getAttribute("transform")),L.setAttribute("width",+E.getAttribute("width")+80),L.setAttribute("height",+E.getAttribute("height")+80),L.setAttribute("x",E.getAttribute("x")-40),L.setAttribute("y",E.getAttribute("y")-40)}}}function hideGraph(){document.getElementById("inheritance-diagram").classList.remove("shown")}function zoomOut(){d3.select("#graph").transition().duration(2e3).call(zoom.transform,transform)}sideMenuToggler.addEventListener("click",(e=>{document.getElementById("leftColumn").classList.toggle("show"),document.getElementById("content").classList.toggle("sidebar-shown");const t=document.getElementById("toc");t&&t.childElementCount>0&&t.classList.toggle("sidebar-shown"),sideMenuToggler.classList.toggle("menu-shown")})),document.getElementById("mobile-menu-toggle").addEventListener("click",(e=>{document.getElementById("mobile-menu").classList.add("show")})),document.getElementById("mobile-menu-close").addEventListener("click",(e=>{document.getElementById("mobile-menu").classList.remove("show")})),window.addEventListener("popstate",(e=>{null!==e.state&&(loadPageState(document,e.state),window.dispatchEvent(new Event(DYNAMIC_PAGE_LOAD)))}));const members=[...document.querySelectorAll("[id]")];members.forEach((e=>{window.addEventListener("resize",(()=>{const t=document.querySelector("#header").clientHeight,n=document.querySelector(".documentableFilter")?.clientHeight;t&&n&&(e.style.scrollMarginTop=`${t+n}px`)}))})),members.forEach((e=>{window.addEventListener("DOMContentLoaded",(()=>{const t=document.querySelector("#header").clientHeight,n=document.querySelector(".documentableFilter")?.clientHeight;t&&n&&(e.style.scrollMarginTop=`${t+n}px`)}))})),window.addEventListener(DYNAMIC_PAGE_LOAD,(()=>{const e=document.querySelector("#docs-nav-button"),t=document.querySelector("#api-nav-button");e&&t&&[e,t].forEach((e=>{e.addEventListener("click",(()=>{sessionStorage.setItem("sideMenuOpen",!0)}))}))}));