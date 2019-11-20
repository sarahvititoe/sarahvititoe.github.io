/* jQuery Colour picker: A tiny colour picker with useful extra features / v1.1 / Dean Attali and Cory LaViska / MIT license */
!function(a){"function"==typeof define&&define.amd?define(["jquery"],a):"object"==typeof exports?module.exports=a(require("jquery")):a(jQuery)}(function(a){function b(b,c){var d=a('<div class="colourpicker" />'),e=a.colourpicker.defaults;if(!b.data("colourpicker-initialized")){if(c=a.extend(!0,{},e,c),d.addClass("palette-"+c.palette),c.allowTransparent&&d.addClass("input-group"),b.addClass("colourpicker-input").prop("spellcheck",!1).data("colourpicker-initialized",!1).data("colourpicker-lastChange",!1).data("colourpicker-settings",c).prop("size",7).wrap(d),"square"==c.palette)b.after('<div class="colourpicker-panel"><div class="colourpicker-slider colourpicker-sprite"><div class="colourpicker-slider-picker"></div></div><div class="colourpicker-grid colourpicker-sprite"><div class="colourpicker-grid-inner"></div><div class="colourpicker-picker"><div></div></div></div></div>');else if("limited"==c.palette){var f='<div class="colourpicker-list">';a.each(c.allowedCols.split(" "),function(a,b){0==a?f+='<div class="cp-list-row cp-clearfix">':a%8==0&&(f+='</div><div class="cp-list-row cp-clearfix">'),f+='<span class="cp-list-col" data-cp-col="'+b+'" style="background-color:'+b+'"></span>'}),f+="</div>",b.after('<div class="colourpicker-panel">'+f+"</div>")}else console.log("colourpicker: invalid palette type ("+c.palette+")");c.allowTransparent?(b.parent().find(".colourpicker-panel").after('<label class="input-group-addon"><input type="checkbox" class="colourpicker-istransparent"> <span class="colourpicker-transparent-text">'+c.transparentText+"</span></label>"),b.data("allow-transparent",!0)):b.data("allow-transparent",!1),"background"==c.showColour&&b.attr("readonly","readonly"),c.returnName?b.data("return-name",!0):b.data("return-name",!1),b.parent().find(".colourpicker-panel").on("selectstart",function(){return!1}).end(),h(b,!1),b.data("colourpicker-initialized",!0)}}function c(a){var b=a.parent();a.removeData("colourpicker-initialized").removeData("colourpicker-settings").removeProp("size").removeClass("colourpicker-input"),b.before(a).remove()}function d(a){var b=a.parent(),c=b.find(".colourpicker-panel"),d=a.data("colourpicker-settings");!a.data("colourpicker-initialized")||a.prop("disabled")||b.hasClass("colourpicker-focus")||(e(),b.addClass("colourpicker-focus"),c.stop(!0,!0).fadeIn(d.showSpeed,function(){d.show&&d.show.call(a.get(0))}))}function e(){a(".colourpicker-focus").each(function(){var b=a(this),c=b.find(".colourpicker-input"),d=b.find(".colourpicker-panel"),e=c.data("colourpicker-settings");d.fadeOut(e.hideSpeed,function(){e.hide&&e.hide.call(c.get(0)),b.removeClass("colourpicker-focus")})})}function f(a,b,c){var d=a.parents(".colourpicker").find(".colourpicker-input"),e=d.data("colourpicker-settings"),f=a.find("[class$=-picker]"),h=a.offset().left,i=a.offset().top,j=Math.round(b.pageX-h),k=Math.round(b.pageY-i),l=c?e.animationSpeed:0;b.originalEvent.changedTouches&&(j=b.originalEvent.changedTouches[0].pageX-h,k=b.originalEvent.changedTouches[0].pageY-i),j<0&&(j=0),k<0&&(k=0),j>a.width()&&(j=a.width()),k>a.height()&&(k=a.height()),a.is(".colourpicker-grid")?f.stop(!0).animate({top:k+"px",left:j+"px"},l,e.animationEasing,function(){g(d,a)}):f.stop(!0).animate({top:k+"px"},l,e.animationEasing,function(){g(d,a)})}function g(a,b){function c(a,b){var c,d;return a.length&&b?(c=a.offset().left,d=a.offset().top,{x:c-b.offset().left+a.outerWidth()/2,y:d-b.offset().top+a.outerHeight()/2}):null}var d,e,f,m=a.val(),n=a.parent(),p=a.data("colourpicker-settings"),q=n.find(".colourpicker-grid"),r=n.find(".colourpicker-slider"),t=q.find("[class$=-picker]"),u=r.find("[class$=-picker]"),v=c(t,q),w=c(u,r);switch(b.is(".colourpicker-grid, .colourpicker-slider")&&(d=l(360-parseInt(w.y*(360/r.height()),10),0,360),e=l(Math.floor(v.x*(100/q.width())),0,100),f=l(100-Math.floor(v.y*(100/q.height())),0,100),m=o({h:d,s:e,b:f}),q.css("backgroundColor",o({h:d,s:100,b:100})),a.val(m.toUpperCase())),b.is(".cp-list-col")&&(m=b.data("cp-col"),a.val(m)),p.showColour){case"text":a.css("color",""),a.css("background-color","");break;case"background":a.css("color","transparent"),a.css("background-color",m);break;default:a.css("color",s(m)?"#ddd":"#000"),a.css("background-color",m)}"limited"==p.palette&&(n.find(".cp-list-col").removeClass("selected-col"),n.find('.cp-list-col[data-cp-col="'+m+'"]').addClass("selected-col").addClass(s(m)?"dark":"light")),i(a,m,a.data("transparent"))}function h(a,b){var c,d,e,f,k=a.parent(),m=a.data("colourpicker-settings"),n=k.find(".colourpicker-grid"),q=k.find(".colourpicker-slider"),r=n.find("[class$=-picker]"),t=q.find("[class$=-picker]");switch(c=j(a),c||(c=u(a)),d=p(c),b||a.val(c),m.showColour){case"text":a.css("color",""),a.css("background-color","");break;case"background":a.css("color","transparent"),a.css("background-color",c);break;default:a.css("color",s(c)?"#ddd":"#000"),a.css("background-color",c)}"limited"==m.palette&&(k.find(".cp-list-col").removeClass("selected-col"),k.find('.cp-list-col[data-cp-col="'+c+'"]').addClass("selected-col").addClass(s(c)?"dark":"light")),e=l(Math.ceil(d.s/(100/n.width())),0,n.width()),f=l(n.height()-Math.ceil(d.b/(100/n.height())),0,n.height()),r.css({top:f+"px",left:e+"px"}),f=l(q.height()-d.h/(360/q.height()),0,q.height()),t.css("top",f+"px"),n.css("backgroundColor",o({h:d.h,s:100,b:100})),a.data("colourpicker-initialized")&&i(a,c,a.data("transparent")),a.data("transparent")?(k.find(".colourpicker-istransparent").prop("checked",!0),k.addClass("istransparent")):(k.find(".colourpicker-istransparent").prop("checked",!1),k.removeClass("istransparent")),a.trigger("change").trigger("input")}function i(a,b,c){var d=a.data("colourpicker-settings"),e=a.data("colourpicker-lastChange"),f=a.data("colourpicker-lastTransparent");e&&e===b&&f===c||(a.data("colourpicker-lastChange",b),a.data("colourpicker-lastTransparent",c),d.change&&(d.changeDelay?(clearTimeout(a.data("colourpicker-changeTimeout")),a.data("colourpicker-changeTimeout",setTimeout(function(){d.change.call(a.get(0),b)},d.changeDelay))):d.change.call(a.get(0),b)),a.trigger("change").trigger("input"))}function j(b){var c=b.val(),d=b.data("colourpicker-settings"),e=k(c).toUpperCase();return"limited"==d.palette&&a.inArray(e,d.allowedCols.split(" "))==-1&&(e=""),e}function k(a){return a=a.replace(/[^A-F0-9]/gi,""),3!==a.length&&6!==a.length?"":(3===a.length&&(a=a[0]+a[0]+a[1]+a[1]+a[2]+a[2]),"#"+a)}function l(a,b,c){return a<b&&(a=b),a>c&&(a=c),a}function m(a){var b={},c=Math.round(a.h),d=Math.round(255*a.s/100),e=Math.round(255*a.b/100);if(0===d)b.r=b.g=b.b=e;else{var f=e,g=(255-d)*e/255,h=(f-g)*(c%60)/60;360===c&&(c=0),c<60?(b.r=f,b.b=g,b.g=g+h):c<120?(b.g=f,b.b=g,b.r=f-h):c<180?(b.g=f,b.r=g,b.b=g+h):c<240?(b.b=f,b.r=g,b.g=f-h):c<300?(b.b=f,b.g=g,b.r=g+h):c<360?(b.r=f,b.g=g,b.b=f-h):(b.r=0,b.g=0,b.b=0)}return{r:Math.round(b.r),g:Math.round(b.g),b:Math.round(b.b)}}function n(b){var c=[b.r.toString(16),b.g.toString(16),b.b.toString(16)];return a.each(c,function(a,b){1===b.length&&(c[a]="0"+b)}),"#"+c.join("")}function o(a){return n(m(a))}function p(a){var b=q(r(a));return 0===b.s&&(b.h=360),b}function q(a){var b={h:0,s:0,b:0},c=Math.min(a.r,a.g,a.b),d=Math.max(a.r,a.g,a.b),e=d-c;return b.b=d,b.s=0!==d?255*e/d:0,0!==b.s?a.r===d?b.h=(a.g-a.b)/e:a.g===d?b.h=2+(a.b-a.r)/e:b.h=4+(a.r-a.g)/e:b.h=-1,b.h*=60,b.h<0&&(b.h+=360),b.s*=100/255,b.b*=100/255,b}function r(a){return a=parseInt(a.indexOf("#")>-1?a.substring(1):a,16),{r:a>>16,g:(65280&a)>>8,b:255&a}}function s(a){return!(t(a)>.22)}function t(b){var c=r(b);c=a.map(c,function(a){return a/=255,a=a<=.03928?a/12.92:Math.pow((a+.055)/1.055,2.4)});var d=.2126*c[0]+.7152*c[1]+.0722*c[2];return d}function u(a){if(a.data("colourpicker-lastChange"))return a.data("colourpicker-lastChange");if(a.parent().is(".palette-limited")){var b=a.parent().find(".cp-list-col").first();return b.data("cp-col")}return"#FFFFFF"}a.colourpicker={defaults:{animationSpeed:50,animationEasing:"swing",change:null,changeDelay:0,hide:null,hideSpeed:100,show:null,showSpeed:100,showColour:"both",allowTransparent:!1,transparentText:"Transparent",palette:"square",allowedCols:"#000000 #333333 #4D4D4D #666666 #7F7F7F #999999 #B3B3B3 #E5E5E5 #FFFFFF #27408B #000080 #0000FF #1E90FF #63B8FF #97FFFF #00FFFF #00868B #008B45 #458B00 #008B00 #00FF00 #7FFF00 #54FF9F #00FF7F #7FFFD4 #8B4500 #8B0000 #FF0000 #FF6A6A #FF7F00 #FFFF00 #FFF68F #F4A460 #551A8B #8B008B #8B0A50 #9400D3 #FF00FF #FF1493 #E066FF",returnName:!1,colsMap:{"#FFFFFF":"white","#F0F8FF":"aliceblue","#FAEBD7":"antiquewhite","#FFEFDB":"antiquewhite1","#EEDFCC":"antiquewhite2","#CDC0B0":"antiquewhite3","#8B8378":"antiquewhite4","#7FFFD4":"aquamarine","#76EEC6":"aquamarine2","#66CDAA":"aquamarine3","#458B74":"aquamarine4","#F0FFFF":"azure","#E0EEEE":"azure2","#C1CDCD":"azure3","#838B8B":"azure4","#F5F5DC":"beige","#FFE4C4":"bisque","#EED5B7":"bisque2","#CDB79E":"bisque3","#8B7D6B":"bisque4","#000000":"black","#FFEBCD":"blanchedalmond","#0000FF":"blue","#0000EE":"blue2","#0000CD":"blue3","#00008B":"blue4","#8A2BE2":"blueviolet","#A52A2A":"brown","#FF4040":"brown1","#EE3B3B":"brown2","#CD3333":"brown3","#8B2323":"brown4","#DEB887":"burlywood","#FFD39B":"burlywood1","#EEC591":"burlywood2","#CDAA7D":"burlywood3","#8B7355":"burlywood4","#5F9EA0":"cadetblue","#98F5FF":"cadetblue1","#8EE5EE":"cadetblue2","#7AC5CD":"cadetblue3","#53868B":"cadetblue4","#7FFF00":"chartreuse","#76EE00":"chartreuse2","#66CD00":"chartreuse3","#458B00":"chartreuse4","#D2691E":"chocolate","#FF7F24":"chocolate1","#EE7621":"chocolate2","#CD661D":"chocolate3","#8B4513":"chocolate4","#FF7F50":"coral","#FF7256":"coral1","#EE6A50":"coral2","#CD5B45":"coral3","#8B3E2F":"coral4","#6495ED":"cornflowerblue","#FFF8DC":"cornsilk","#EEE8CD":"cornsilk2","#CDC8B1":"cornsilk3","#8B8878":"cornsilk4","#00FFFF":"cyan","#00EEEE":"cyan2","#00CDCD":"cyan3","#008B8B":"cyan4","#B8860B":"darkgoldenrod","#FFB90F":"darkgoldenrod1","#EEAD0E":"darkgoldenrod2","#CD950C":"darkgoldenrod3","#8B6508":"darkgoldenrod4","#A9A9A9":"darkgray","#006400":"darkgreen","#BDB76B":"darkkhaki","#8B008B":"darkmagenta","#556B2F":"darkolivegreen","#CAFF70":"darkolivegreen1","#BCEE68":"darkolivegreen2","#A2CD5A":"darkolivegreen3","#6E8B3D":"darkolivegreen4","#FF8C00":"darkorange","#FF7F00":"darkorange1","#EE7600":"darkorange2","#CD6600":"darkorange3","#8B4500":"darkorange4","#9932CC":"darkorchid","#BF3EFF":"darkorchid1","#B23AEE":"darkorchid2","#9A32CD":"darkorchid3","#68228B":"darkorchid4","#8B0000":"darkred","#E9967A":"darksalmon","#8FBC8F":"darkseagreen","#C1FFC1":"darkseagreen1","#B4EEB4":"darkseagreen2","#9BCD9B":"darkseagreen3","#698B69":"darkseagreen4","#483D8B":"darkslateblue","#2F4F4F":"darkslategray","#97FFFF":"darkslategray1","#8DEEEE":"darkslategray2","#79CDCD":"darkslategray3","#528B8B":"darkslategray4","#00CED1":"darkturquoise","#9400D3":"darkviolet","#FF1493":"deeppink","#EE1289":"deeppink2","#CD1076":"deeppink3","#8B0A50":"deeppink4","#00BFFF":"deepskyblue","#00B2EE":"deepskyblue2","#009ACD":"deepskyblue3","#00688B":"deepskyblue4","#696969":"dimgray","#1E90FF":"dodgerblue","#1C86EE":"dodgerblue2","#1874CD":"dodgerblue3","#104E8B":"dodgerblue4","#B22222":"firebrick","#FF3030":"firebrick1","#EE2C2C":"firebrick2","#CD2626":"firebrick3","#8B1A1A":"firebrick4","#FFFAF0":"floralwhite","#228B22":"forestgreen","#DCDCDC":"gainsboro","#F8F8FF":"ghostwhite","#FFD700":"gold","#EEC900":"gold2","#CDAD00":"gold3","#8B7500":"gold4","#DAA520":"goldenrod","#FFC125":"goldenrod1","#EEB422":"goldenrod2","#CD9B1D":"goldenrod3","#8B6914":"goldenrod4","#BEBEBE":"gray","#030303":"gray1","#050505":"gray2","#080808":"gray3","#0A0A0A":"gray4","#0D0D0D":"gray5","#0F0F0F":"gray6","#121212":"gray7","#141414":"gray8","#171717":"gray9","#1A1A1A":"gray10","#1C1C1C":"gray11","#1F1F1F":"gray12","#212121":"gray13","#242424":"gray14","#262626":"gray15","#292929":"gray16","#2B2B2B":"gray17","#2E2E2E":"gray18","#303030":"gray19","#333333":"gray20","#363636":"gray21","#383838":"gray22","#3B3B3B":"gray23","#3D3D3D":"gray24","#404040":"gray25","#424242":"gray26","#454545":"gray27","#474747":"gray28","#4A4A4A":"gray29","#4D4D4D":"gray30","#4F4F4F":"gray31","#525252":"gray32","#545454":"gray33","#575757":"gray34","#595959":"gray35","#5C5C5C":"gray36","#5E5E5E":"gray37","#616161":"gray38","#636363":"gray39","#666666":"gray40","#6B6B6B":"gray42","#6E6E6E":"gray43","#707070":"gray44","#737373":"gray45","#757575":"gray46","#787878":"gray47","#7A7A7A":"gray48","#7D7D7D":"gray49","#7F7F7F":"gray50","#828282":"gray51","#858585":"gray52","#878787":"gray53","#8A8A8A":"gray54","#8C8C8C":"gray55","#8F8F8F":"gray56","#919191":"gray57","#949494":"gray58","#969696":"gray59","#999999":"gray60","#9C9C9C":"gray61","#9E9E9E":"gray62","#A1A1A1":"gray63","#A3A3A3":"gray64","#A6A6A6":"gray65","#A8A8A8":"gray66","#ABABAB":"gray67","#ADADAD":"gray68","#B0B0B0":"gray69","#B3B3B3":"gray70","#B5B5B5":"gray71","#B8B8B8":"gray72","#BABABA":"gray73","#BDBDBD":"gray74","#BFBFBF":"gray75","#C2C2C2":"gray76","#C4C4C4":"gray77","#C7C7C7":"gray78","#C9C9C9":"gray79","#CCCCCC":"gray80","#CFCFCF":"gray81","#D1D1D1":"gray82","#D4D4D4":"gray83","#D6D6D6":"gray84","#D9D9D9":"gray85","#DBDBDB":"gray86","#DEDEDE":"gray87","#E0E0E0":"gray88","#E3E3E3":"gray89","#E5E5E5":"gray90","#E8E8E8":"gray91","#EBEBEB":"gray92","#EDEDED":"gray93","#F0F0F0":"gray94","#F2F2F2":"gray95","#F5F5F5":"gray96","#F7F7F7":"gray97","#FAFAFA":"gray98","#FCFCFC":"gray99","#00FF00":"green","#00EE00":"green2","#00CD00":"green3","#008B00":"green4","#ADFF2F":"greenyellow","#F0FFF0":"honeydew","#E0EEE0":"honeydew2","#C1CDC1":"honeydew3","#838B83":"honeydew4","#FF69B4":"hotpink","#FF6EB4":"hotpink1","#EE6AA7":"hotpink2","#CD6090":"hotpink3","#8B3A62":"hotpink4","#CD5C5C":"indianred","#FF6A6A":"indianred1","#EE6363":"indianred2","#CD5555":"indianred3","#8B3A3A":"indianred4","#FFFFF0":"ivory","#EEEEE0":"ivory2","#CDCDC1":"ivory3","#8B8B83":"ivory4","#F0E68C":"khaki","#FFF68F":"khaki1","#EEE685":"khaki2","#CDC673":"khaki3","#8B864E":"khaki4","#E6E6FA":"lavender","#FFF0F5":"lavenderblush","#EEE0E5":"lavenderblush2","#CDC1C5":"lavenderblush3","#8B8386":"lavenderblush4","#7CFC00":"lawngreen","#FFFACD":"lemonchiffon","#EEE9BF":"lemonchiffon2","#CDC9A5":"lemonchiffon3","#8B8970":"lemonchiffon4","#ADD8E6":"lightblue","#BFEFFF":"lightblue1","#B2DFEE":"lightblue2","#9AC0CD":"lightblue3","#68838B":"lightblue4","#F08080":"lightcoral","#E0FFFF":"lightcyan","#D1EEEE":"lightcyan2","#B4CDCD":"lightcyan3","#7A8B8B":"lightcyan4","#EEDD82":"lightgoldenrod","#FFEC8B":"lightgoldenrod1","#EEDC82":"lightgoldenrod2","#CDBE70":"lightgoldenrod3","#8B814C":"lightgoldenrod4","#FAFAD2":"lightgoldenrodyellow","#D3D3D3":"lightgray","#90EE90":"lightgreen","#FFB6C1":"lightpink","#FFAEB9":"lightpink1","#EEA2AD":"lightpink2","#CD8C95":"lightpink3","#8B5F65":"lightpink4","#FFA07A":"lightsalmon","#EE9572":"lightsalmon2","#CD8162":"lightsalmon3","#8B5742":"lightsalmon4","#20B2AA":"lightseagreen","#87CEFA":"lightskyblue","#B0E2FF":"lightskyblue1","#A4D3EE":"lightskyblue2","#8DB6CD":"lightskyblue3","#607B8B":"lightskyblue4","#8470FF":"lightslateblue","#778899":"lightslategray","#B0C4DE":"lightsteelblue","#CAE1FF":"lightsteelblue1","#BCD2EE":"lightsteelblue2","#A2B5CD":"lightsteelblue3","#6E7B8B":"lightsteelblue4","#FFFFE0":"lightyellow","#EEEED1":"lightyellow2","#CDCDB4":"lightyellow3","#8B8B7A":"lightyellow4","#32CD32":"limegreen","#FAF0E6":"linen","#FF00FF":"magenta","#EE00EE":"magenta2","#CD00CD":"magenta3","#B03060":"maroon","#FF34B3":"maroon1","#EE30A7":"maroon2","#CD2990":"maroon3","#8B1C62":"maroon4","#BA55D3":"mediumorchid","#E066FF":"mediumorchid1","#D15FEE":"mediumorchid2","#B452CD":"mediumorchid3","#7A378B":"mediumorchid4","#9370DB":"mediumpurple","#AB82FF":"mediumpurple1","#9F79EE":"mediumpurple2","#8968CD":"mediumpurple3","#5D478B":"mediumpurple4","#3CB371":"mediumseagreen","#7B68EE":"mediumslateblue","#00FA9A":"mediumspringgreen","#48D1CC":"mediumturquoise","#C71585":"mediumvioletred","#191970":"midnightblue","#F5FFFA":"mintcream","#FFE4E1":"mistyrose","#EED5D2":"mistyrose2","#CDB7B5":"mistyrose3","#8B7D7B":"mistyrose4","#FFE4B5":"moccasin","#FFDEAD":"navajowhite","#EECFA1":"navajowhite2","#CDB38B":"navajowhite3","#8B795E":"navajowhite4","#000080":"navy","#FDF5E6":"oldlace","#6B8E23":"olivedrab","#C0FF3E":"olivedrab1","#B3EE3A":"olivedrab2","#9ACD32":"olivedrab3","#698B22":"olivedrab4","#FFA500":"orange","#EE9A00":"orange2","#CD8500":"orange3","#8B5A00":"orange4","#FF4500":"orangered","#EE4000":"orangered2","#CD3700":"orangered3","#8B2500":"orangered4","#DA70D6":"orchid","#FF83FA":"orchid1","#EE7AE9":"orchid2","#CD69C9":"orchid3","#8B4789":"orchid4","#EEE8AA":"palegoldenrod","#98FB98":"palegreen","#9AFF9A":"palegreen1","#7CCD7C":"palegreen3","#548B54":"palegreen4","#AFEEEE":"paleturquoise","#BBFFFF":"paleturquoise1","#AEEEEE":"paleturquoise2","#96CDCD":"paleturquoise3","#668B8B":"paleturquoise4","#DB7093":"palevioletred","#FF82AB":"palevioletred1","#EE799F":"palevioletred2","#CD6889":"palevioletred3","#8B475D":"palevioletred4","#FFEFD5":"papayawhip","#FFDAB9":"peachpuff","#EECBAD":"peachpuff2","#CDAF95":"peachpuff3","#8B7765":"peachpuff4","#CD853F":"peru","#FFC0CB":"pink","#FFB5C5":"pink1","#EEA9B8":"pink2","#CD919E":"pink3","#8B636C":"pink4","#DDA0DD":"plum","#FFBBFF":"plum1","#EEAEEE":"plum2","#CD96CD":"plum3","#8B668B":"plum4","#B0E0E6":"powderblue","#A020F0":"purple","#9B30FF":"purple1","#912CEE":"purple2","#7D26CD":"purple3","#551A8B":"purple4","#FF0000":"red","#EE0000":"red2","#CD0000":"red3","#BC8F8F":"rosybrown","#FFC1C1":"rosybrown1","#EEB4B4":"rosybrown2","#CD9B9B":"rosybrown3","#8B6969":"rosybrown4","#4169E1":"royalblue","#4876FF":"royalblue1","#436EEE":"royalblue2","#3A5FCD":"royalblue3","#27408B":"royalblue4","#FA8072":"salmon","#FF8C69":"salmon1","#EE8262":"salmon2","#CD7054":"salmon3","#8B4C39":"salmon4","#F4A460":"sandybrown","#2E8B57":"seagreen","#54FF9F":"seagreen1","#4EEE94":"seagreen2","#43CD80":"seagreen3","#FFF5EE":"seashell","#EEE5DE":"seashell2","#CDC5BF":"seashell3","#8B8682":"seashell4","#A0522D":"sienna","#FF8247":"sienna1","#EE7942":"sienna2","#CD6839":"sienna3","#8B4726":"sienna4","#87CEEB":"skyblue","#87CEFF":"skyblue1","#7EC0EE":"skyblue2","#6CA6CD":"skyblue3","#4A708B":"skyblue4","#6A5ACD":"slateblue","#836FFF":"slateblue1","#7A67EE":"slateblue2","#6959CD":"slateblue3","#473C8B":"slateblue4","#708090":"slategray","#C6E2FF":"slategray1","#B9D3EE":"slategray2","#9FB6CD":"slategray3","#6C7B8B":"slategray4","#FFFAFA":"snow","#EEE9E9":"snow2","#CDC9C9":"snow3","#8B8989":"snow4","#00FF7F":"springgreen","#00EE76":"springgreen2","#00CD66":"springgreen3","#008B45":"springgreen4","#4682B4":"steelblue","#63B8FF":"steelblue1","#5CACEE":"steelblue2","#4F94CD":"steelblue3","#36648B":"steelblue4","#D2B48C":"tan","#FFA54F":"tan1","#EE9A49":"tan2","#8B5A2B":"tan4","#D8BFD8":"thistle","#FFE1FF":"thistle1","#EED2EE":"thistle2","#CDB5CD":"thistle3","#8B7B8B":"thistle4","#FF6347":"tomato","#EE5C42":"tomato2","#CD4F39":"tomato3","#8B3626":"tomato4","#40E0D0":"turquoise","#00F5FF":"turquoise1","#00E5EE":"turquoise2","#00C5CD":"turquoise3","#00868B":"turquoise4","#EE82EE":"violet","#D02090":"violetred","#FF3E96":"violetred1","#EE3A8C":"violetred2","#CD3278":"violetred3","#8B2252":"violetred4","#F5DEB3":"wheat","#FFE7BA":"wheat1","#EED8AE":"wheat2","#CDBA96":"wheat3","#8B7E66":"wheat4","#FFFF00":"yellow","#EEEE00":"yellow2","#CDCD00":"yellow3","#8B8B00":"yellow4"}}},a.extend(a.fn,{colourpicker:function(f,g){switch(f){case"destroy":return a(this).each(function(){c(a(this))}),a(this);case"hide":return e(),a(this);case"settings":return void 0===g?a(this).data("colourpicker-settings"):(a(this).each(function(){var b=a(this).data("colourpicker-settings")||{};c(a(this)),a(this).colourpicker(a.extend(!0,b,g))}),a(this));case"show":return d(a(this).eq(0)),a(this);case"value":if(void 0===g){if(a(this).data("allow-transparent")&&a(this).data("transparent"))return"transparent";if(!j(a(this)))return a(this).data("colourpicker-lastChange");var i=j(a(this));return a(this).data("return-name")&&i in a(this).data("colourpicker-settings").colsMap&&(i=a(this).data("colourpicker-settings").colsMap[i]),i}return a(this).each(function(){"transparent"==g?a(this).data("allow-transparent")?a(this).data("transparent",!0):(a(this).data("transparent",!1),a(this).val(u(a(this)))):(a(this).data("transparent",!1),a(this).val(g)),h(a(this))}),a(this);default:return"create"!==f&&(g=f),a(this).each(function(){b(a(this),g)}),a(this)}}}),a(document).on("mousedown.colourpicker touchstart.colourpicker",function(b){a(b.target).parents().add(b.target).hasClass("colourpicker")||e()}).on("mousedown.colourpicker touchstart.colourpicker",".cp-list-col",function(b){var c=a(this);b.preventDefault();var d=c.parents(".colourpicker").find(".colourpicker-input");g(d,c)}).on("mousedown.colourpicker touchstart.colourpicker",".colourpicker-grid, .colourpicker-slider",function(b){var c=a(this);b.preventDefault(),a(document).data("colourpicker-target",c),f(c,b,!0)}).on("mousemove.colourpicker touchmove.colourpicker",function(b){var c=a(document).data("colourpicker-target");c&&f(c,b)}).on("mouseup.colourpicker touchend.colourpicker",function(){a(this).removeData("colourpicker-target")}).on("focus.colourpicker",".colourpicker-input",function(){var b=a(this);b.data("colourpicker-initialized")&&(b.data("transparent",!1),d(b),h(a(this)))}).on("blur.colourpicker",".colourpicker-input",function(){var b=a(this);b.data("colourpicker-settings");b.data("colourpicker-initialized")&&(b.val(j(b)),""===b.val()&&b.val(u(b)),b.val(b.val().toUpperCase()))}).on("keydown.colourpicker",".colourpicker-input",function(b){var c=a(this);if(c.data("colourpicker-initialized"))switch(b.keyCode){case 9:e();break;case 13:case 27:e(),c.blur()}}).on("keyup.colourpicker",".colourpicker-input",function(){var b=a(this);b.data("colourpicker-initialized")&&h(b,!0)}).on("paste.colourpicker",".colourpicker-input",function(){var b=a(this);b.data("colourpicker-initialized")&&setTimeout(function(){h(b,!0)},1)}).on("change.colourpicker-istransparent",".input-group-addon",function(b){var c=a(this).siblings(".colourpicker-input"),f=(c.parent(),a(this).find(".colourpicker-istransparent"));e(),c.data("transparent",f.is(":checked")),h(c)})});
