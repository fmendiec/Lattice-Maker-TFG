var latticeMaker = function( id_textarea, id_canvas ) {

	// Obtener posición del cursor
	var getMousePosition = function( ) {
		if( typeof( event ) == "undefined" || event === false ) {
			return false;
		}
		var rect = canvas.getBoundingClientRect();
		return {
			x: event.clientX - rect.left,
			y: event.clientY - rect.top
		};
	};

	// Comenzar a arrastrar miembro
	var drag_start = function( ) {
		if( hover !== false ) {
			action = 1;
		}
	};

	// Dejar de arrastrar miembro
	var drag_stop = function( ) {
		action = 0;
	};

	// Actualizar tamaño
	var resize = function( ) {
		oheight = height;
		owidth = width;
		width = jtextarea.width() + 20;
		height = jtextarea.height() + 20;
		if( Math.abs(owidth - width) > 10 || oheight != height ) {
			jcanvas.css( "width", width + "px" );
			jcanvas.css( "height", height + "px" );
			canvas.width = width;
			canvas.height = height;
			sort();
			update();
		}
	};

	// Pintar fondo
	var background = function( ) {
		ctx.beginPath();
		ctx.rect( 0, 0, width, height );
		ctx.fillStyle = "#efefef";
		ctx.fill();
		ctx.lineWidth = 1;
		ctx.strokeStyle = "#dfdfdf";
		for( var i = 1; i < width / 50; i++ ) {
			ctx.beginPath();
			ctx.moveTo( i * 50, 0 );
			ctx.lineTo( i * 50, height );
			ctx.stroke();
		}
		for( var i = 1; i < height / 50; i++ ) {
			ctx.beginPath();
			ctx.moveTo( 0, i * 50 );
			ctx.lineTo( width, i * 50 );
			ctx.stroke();
		}
	};

	// Pintar un miembro
	var drawMember = function( member ) {
		pos = getMousePosition();
		if( action == 1 && hover === member.member || pos !== false && hover === false && Math.sqrt( Math.pow(member.x - pos.x, 2) + Math.pow(member.y - pos.y, 2) ) < radius ) {
			hover = member.member;
			ctx.fillStyle = colors[member.color + "_hover"];
			if( action === 1) {	
				member.x = pos.x;
				member.y = pos.y;
			}
			jcanvas.css( "cursor", "pointer" );
		} else {
			ctx.fillStyle = colors[member.color];
		}
		ctx.lineWidth = 2;
		ctx.strokeStyle = "#000000";
		ctx.beginPath();
		var tmp_radius = radius;
		if( member.member == hover ) { tmp_radius = radius_hover; }
		ctx.arc( member.x, member.y, tmp_radius, 0, 2 * Math.PI );
		ctx.fill();
		ctx.stroke();
		ctx.font = "bold 12px Roboto Mono, Monospace, Courier New";
		ctx.fillStyle = "#000000";
		ctx.textAlign = "center";
		ctx.fillText(member.member, member.x, member.y + 5);
	};

	// Pintar miembros
	var drawMembers = function( ) {
		if( action === 0) {
			hover = false;
			jcanvas.css( "cursor", "default" );
		}
		for( var i = 0; i < members.length; i++ ) {
			drawMember( members[i] );
		}
	};

	// Pintar un arco
	var drawArc = function( arc ) {
		var from = lookupMember( arc.from );
		var to = lookupMember( arc.to );
		if( from !== false && to !== false ) {
			var tmp_radius = radius;
            // Arrow lenght and angle
            var arrowLen = 35;
            var arrowAng = 10;
            
			if( from.member == hover ) { arrowLen = 40; arrowAng = 15; }
			var alpha = Math.atan( Math.abs( from.y - to.y ) / Math.abs( from.x - to.x ) );
			var x = tmp_radius * Math.cos(alpha);
			var y = tmp_radius * Math.sin(alpha);
            
            // Arrow head is down or top
            var downx = -1,downy = -1;
            
			if( from.x > to.x ) { x = -x; downx = -downx; }
			if( from.y > to.y ) { y = -y; downy = -downy; }
			ctx.lineWidth = 2;
			ctx.fillStyle = "#000000";
			ctx.strokeStyle = "#000000";
			ctx.beginPath();
            
            // Extremes of the arrow in radians
            var end1 = alpha + (180+arrowAng) * Math.PI/180
            var end2 = alpha + (180-arrowAng) * Math.PI/180
            
            // Arrow Head points
            var x1 = to.x - downx * (arrowLen * Math.cos(end1))
            var y1 = to.y - downy * (arrowLen * Math.sin(end1))
            var x2 = to.x - downx * (arrowLen * Math.cos(end2))
            var y2 = to.y - downy * (arrowLen * Math.sin(end2))            
            
            // Draw the arrow
            ctx.moveTo(to.x-x, to.y-y);
            ctx.lineTo(x1,y1);
            ctx.moveTo(to.x-x, to.y-y);
            ctx.lineTo(x2,y2);
            
            ctx.arcTo(x1,y1,x2,y2,35);
			ctx.fill();
			ctx.stroke();
            
			ctx.beginPath();
			ctx.moveTo( from.x, from.y );
			ctx.lineTo( to.x, to.y );
			ctx.stroke();
            
            
		}
	};

	// Pintar arcos
	var drawArcs = function( ) {
		for( var i = 0; i < arcs.length; i++ ) {
			drawArc( arcs[i] );
		}
	};

	// Descubrir miembro
	var lookupMember = function( member ) {
		for( var i = 0; i < members.length; i++ ) {
			if( members[i].member == member ) {
				return members[i];
			}
		}
		return false;
	};

	// Descubrir arco
	var lookupArc = function( from, to ) {
		for( var i = 0; i < arcs.length; i++ ) {
			if( arcs[i].from == from && arcs[i].to == to ) {
				return arcs[i];
			}
		}
		return false;
	};

	// Actualizar canvas
	var update = function( e ) {
		event = e;
		background();
		drawArcs();
		drawMembers();
	};

	// Obtener miembros desde el texto
	var getMembersByTextarea = function( ) {
		var code = jtextarea.val();
		var regex = /member\s*\(\s*(.+)\s*\)\s*\./g;
		var matches = true;
		var read = [];
		while( matches = regex.exec( code ) ) {
			read.push( matches[1] );
		}
		var cpmembers = [];
		for( var i = 0; i < read.length; i++ ) {
			lookup = lookupMember( read[i] );
			if( lookup === false ) {
				cpmembers.push( new latticeMember( read[i], Math.random() * width, Math.random() * height ) );
			} else {
				cpmembers.push( lookup );
			}
		}
		members = cpmembers;
	};

	// Obtener arcos desde el texto
	var getArcsByTextarea = function( ) {
		var code = jtextarea.val();
		var regex = /arc\s*\(\s*(.+,.+)\s*\)\s*\./g;
		var matches = true;
		var read = [];
		while( matches = regex.exec( code ) ) {
			var tmp = matches[1];
			var count = 0;
			var comma = -1;
			for( var i = 0; i < tmp.length; i++ ) {
				switch( tmp.charAt( i ) ) {
					case '(':
						count++;
						break;
					case ')':
						count--;
						break;
					case ',':
						if( count == 0 ) { comma = i; }
						break;
				}
				if( comma > 0 ) { break; }
			}
			if( comma > 0 ) {
				read.push( [tmp.substring( 0, comma ).trim(), tmp.substring( comma + 1 ).trim()] );
			}
		}
		var cparcs = [];
		for( var i = 0; i < read.length; i++ ) {
			var lookup = lookupArc( read[i][0], read[i][1] );
			var tmp_arcs = arcs;
			arcs = cparcs;
			var addcycle = cycle( read[i][0], read[i][1] );
			arcs = tmp_arcs;
			if( lookup === false && !addcycle ) {
				cparcs.push( new latticeArc( read[i][0], read[i][1] ) );
			} else {
				cparcs.push( lookup );
			}
		}
		arcs = cparcs;
	};

	// Obtener supremo desde el texto
	var getTopByTextarea = function( ) {
		var code = jtextarea.val();
		var regex = /top\s*\(\s*(.+)\s*\)\s*\./g;
		matches = regex.exec( code );
		if( matches ) {
			top = matches[1];
		} else {
			top = false;
		}
	};

	// Obtener ínfimo desde el texto
	var getBotByTextarea = function( ) {
		var code = jtextarea.val();
		var regex = /bot\s*\(\s*(.+)\s*\)\s*\./g;
		matches = regex.exec( code );
		if( matches ) {
			bottom = matches[1];
		} else {
			bottom = false;
		}
	};

	// Comprobar si un miembro no está conectado con el supremo
	var detached_top = function( member ) {
		return top === false || path( member, top ) === false;
	};

	// Comprobar si un miembro no está conectado con el ínfimo
	var detached_bottom = function( member ) {
		return bottom === false || path( bottom, member ) === false;
	};

	// Comprobar si existe un camino entre dos nodos
	var path = function( from, to ) {
		if( lookupArc( from, to ) !== false ) {
			return true;
		}
		for( var i = 0; i < members.length; i++ ) {
			if( lookupArc( from, members[i].member ) && path( members[i].member, to ) ) {
				return true;
			}
		}
		return false;
	};

	// Actualizar color de miembros
	var updateMemberColors = function() {
		for( var i = 0; i < members.length; i++ ) {
			switch( members[i].member ) {
				case top: members[i].color = "blue"; break;
				case bottom: members[i].color = "yellow"; break;
				default:
					dtop = detached_top( members[i].member );
					dbot = detached_bottom( members[i].member );
					if( dtop && dbot ) {
						members[i].color = "red";
					} else if ( dtop ) {
						members[i].color = "orange";
					} else if ( dbot ) {
						members[i].color  = "purple";
					} else {
						members[i].color = "green";
					}
			}
		}
	};

	// Reordenar miembros
	var sort = function() {
		if( bottom === false || top === false ) {
			return false;
		}
		var levels = getMembersByLevels();
		var hmargin = 0;
		var vmargin = height / (levels.length + 1);
		var offset_x = 0;
		var offset_y = vmargin;
		var all = [];
		for( var i = levels.length - 1; i  >= 0; i-- ) {
			levels[i] = levels[i].filter( function( a ){ return all.indexOf( a ) === -1; } );
			hmargin = width / (levels[i].length + 1);
			offset_x = hmargin;
			for( var j = 0; j < levels[i].length; j++ ) {
				all.push( levels[i][j] );
				lookup = lookupMember( levels[i][j] );
				lookup.x = offset_x;
				lookup.y = offset_y;
				offset_x += hmargin;
			}
			offset_y += vmargin;
		}
		var offset_x = 2 * radius;
		var offset_y = 2 * radius;
		for( var i = 0; i < members.length; i++ ) {
			if( all.indexOf( members[i].member ) === -1 ) {
				all.push( members[i].member );
				members[i].x = offset_x;
				members[i].y = offset_y;
				offset_x += 3 * radius;
			}
		}
	};

	// Obtener profundidad del grafo desde el miembro
	var getDepth = function( member ) {
		if( bottom === false || top === false ) {
			return -1;
		}
		if( member === top ) {
			return 1;
		}
		var maximum = 0;
		for( var i = 0; i < arcs.length; i++ ) {
			if( arcs[i].from === member ) {
				depth = getDepth( arcs[i].to );
				if( depth > maximum ) {
					maximum = depth;
				}
			}
		}
		return 1 + maximum;
	};

	// Obtener miembros de un nivel desde abajo
	var getMembersDownTop = function( level ) {
		if( bottom === false || top === false ) {
			return false;
		}
		if( level <= 0 ) {
			return [bottom];
		}
		var prev = getMembersDownTop( level - 1 );
		var current = [];
		for( var i = 0; i < arcs.length; i++ ) {
			if( prev.indexOf( arcs[i].from ) !== -1 && current.indexOf( arcs[i].to ) === -1 && lookupMember( arcs[i].to ) !== false ) {
				current.push( arcs[i].to );
			}
		}
		return current;
	};

	// Obtener miembros de un nivel desde arriba
	var getMembersTopDown = function( level ) {
		if( bottom === false || top === false ) {
			return false;
		}
		if( level >= getDepth( bottom ) - 1 ) {
			return [top];
		}
		var next = getMembersTopDown( level + 1 );
		var current = [];
		for( var i = 0; i < arcs.length; i++ ) {
			if( next.indexOf( arcs[i].to ) !== -1 && current.indexOf( arcs[i].from ) === -1 && lookupMember( arcs[i].from ) !== false ) {
				current.push( arcs[i].from );
			}
		}
		return current;
	};

	// Obtener miembros por niveles
	var getMembersByLevels = function() {
		if( bottom === false || top === false ) {
			return false;
		}
		var depth = getDepth( bottom );
		var levels = [];
		for( var i = 0; i < depth; i++ ) {
			var dt = getMembersDownTop(i);
			var td = getMembersTopDown(i);
			for( var j = 0; j < td.length; j++ ) {
				if( dt.indexOf( td[j] ) === -1 ) {
					dt.push( td[j] );
				}
			}
			levels.push( dt );
		}
		return levels;
	};

	// Comprobar si un arco añade un ciclo
	var cycle = function( from, to ) {
		return path( to, from );
	};

	// Actualizar desde texto
	var updateByText = function( ) {
		getMembersByTextarea();
		getArcsByTextarea();
		getTopByTextarea();
		getBotByTextarea();
		updateMemberColors();
		sort();
		update( false );	
	};

	// Elementos
    members = [];
	var arcs = [];
	var top = false;
	var bottom = false;
	var hover = false;
	var event = false;
	var action = 0;

	// Colores
	var radius = 24;
	var radius_hover = 30;
	var colors = [];
	colors["red"] = "#ff0000";
	colors["green"] = "#39c33c";
	colors["yellow"] = "#fffc00";
	colors["orange"] = "#ff8a00";
	colors["purple"] = "#b747e1";
	colors["blue"] = "#398ac3";
	colors["red_hover"] = "#ff5757";
	colors["green_hover"] = "#78c379";
	colors["yellow_hover"] = "#fffd67";
	colors["orange_hover"] = "#ffb45b";
	colors["purple_hover"] = "#c886e1";
	colors["blue_hover"] = "#7fa7c3";

	// Obtener elementos
	var textarea = document.getElementById( id_textarea );
	var canvas = document.getElementById( id_canvas );
	var jtextarea = jQuery( "#" + id_textarea );
	var jcanvas = jQuery( "#" + id_canvas );

	// Obtener contexto
	var ctx = canvas.getContext( "2d" );

	// Tamaño del canvas
	var height = 0;
	var width = 0;
	resize();

	// Agregar eventos
	jtextarea.bind( "mouseup", resize );
	jQuery( window ).bind( "resize", resize );
	jtextarea.bind( "change inpuntText input", updateByText );
	jcanvas.bind( "mousemove", function( event ){ update( event ); } );
	jcanvas.bind( "mousedown", drag_start );
	jcanvas.bind( "mouseup", drag_stop );

	// Estado inicial
	updateByText();
};

var latticeMember = function( member, x, y ) {
	this.member = member;
	this.x = x;
	this.y = y;
	this.color = "red";
};

var latticeArc = function( from, to ) {
	this.from = from;
	this.to = to;
};


// BUTTONS

var addMember = function(idTextArea)
{
    
    // Load TextArea
    var textarea = document.getElementById(idTextArea);
    
    // Read the new member's name
    var newMember = prompt("What is the name of the new member?");
    
    // Cancel
    if (!newMember)
        return;
    
    // Error
    if (newMember == '')
    {
        alert('ERROR: Member must have a name.');
        return;
    }
    
    // Not valid characters
    if (newMember.indexOf(',') != -1 || newMember.indexOf('(') != -1 || newMember.indexOf(')') != -1)
    {
        alert('ERROR: Member cannot contain \',\', \'(\' or \')\'')
        return;
    }
        
    if (members.filter(function(m){ return m.member == newMember; }).length == 0)
    {
        // Get the index of the last member
        var code = textarea.value;
        var regex_member = /member\s*\(\s*(.+)\s*\)\s*\./g;
        var match = true;
        var index = 0;
        while(match = regex_member.exec(code)){index = match.index + match[0].length};

        var member = 'member('+newMember+').'

        // Append the new member to TextArea
        code = [code.slice(0,index), member, code.slice(index+1)].join('\n');

        // Get the index of the members list
        var regex_members = /\[\s*(.+)\s*\]\s*\)\s*\./g;
        match = regex_members.exec(code);

        // Create Members section
        if (!match)
        {
            index += member.length;
            code = [code.slice(0,index+1), 'members(['+newMember+']).', code.slice(index+1)].join('\n'); 
        }

        // Add to members section
        else
        {
            index = match.index + match[1].length + 1;
            // Append the new member to the list in TextArea
            code = [code.slice(0,index), ','+newMember, code.slice(index)].join('');
        }
        
        textarea.value = code;
        // Updating TextArea by triggering the event "change"
        $("#"+idTextArea).trigger('change');
    }
}

var removeMember = function(idCanvas,idTextArea)
{    
    // Load Canvas
    var canvas = document.getElementById(idCanvas);
   
    var selected = null;

    // When user click on canvas
    $("#"+idCanvas).one("click",function(event){    
        
        // Get selected member
        selected = detectSelectedMember(canvas,event);    
        
        // Member selected
        if (selected)
            deleteMember(idTextArea,selected.member);
        
        else
            alert('Member not selected') 
    });
    
    // Delete member predicates
    var deleteMember = function(idTextArea,member) 
    {
         // Load TextArea
        var textarea = document.getElementById(idTextArea);
        var code = textarea.value;
        
        // Delete member predicates
        var regex_member = new RegExp("member\\s*\\(\\s*"+member+"\\s*\\)\\s*\\.","g");
        code = code.replace(regex_member,'\r');
        
        // Delete arcs, top and bottom predicates
        var regex_arc = new RegExp("arc\\s*\\(\\s*("+member+"\\s*,.+)|(.+,\\s*"+member+")\\s*\\)\\s*\\.","g");
        code = code.replace(regex_arc,'\r');
        
        // Delete from members list
        // Get member list
        var regex_members = /members\s*\(\s*\[\s*(.*)\s*\]\s*\)\s*\./g;
        var match = code.match(regex_members);
        
        // Member to be delete from list
        var memberBegin = new RegExp("\\[\\s*"+member+"\\s*,","g");
        var memberMiddle = new RegExp(",\\s*"+member+"\\s*,","g");
        var memberEnd = new RegExp(",\\s*"+member+"\\]","g");
        
        // Update List
        
        var newMem = match[0].replace(memberBegin,"\[");
        newMem = newMem.replace(memberMiddle,",");
        newMem = newMem.replace(memberEnd,"\]");
        
        code = code.replace(match,newMem);
        
        // Update Text and Graph
        textarea.value = code;
        $("#"+idTextArea).trigger('change');
    }
    
}


var detectSelectedMember = function(canvas,event)
{
    var chosen = null;
    var rect = canvas.getBoundingClientRect(); 
    
    // Get mouse position
    posx = event.clientX - rect.left;
    posy = event.clientY - rect.top;

    for (member of members)
    {
        // Mouse position is within a member
        if (Math.sqrt( Math.pow(member.x - pos.x, 2) + Math.pow(member.y - pos.y, 2) ) < 24)
        {   
            chosen = member;
            break;
        }
    };    
    
    return chosen;
}

var createArc = function(idCanvas,idTextArea)
{
    var addArc = function(member1,member2)
    {   
        // Load TextArea
        var textarea = document.getElementById(idTextArea);
        var code = textarea.value;
        
        // Arc formed by the two members
        var regex_exarc = new RegExp("arc\\s*\\(\\s*(("+member1+"\\s*,\\s*"+member2+")\|("+member2+"\\s*,\\s*"+member1+"))\\s*\\)\\s*\\.","g");
        
        // If the arc exists, don't write it
        if (!code.match(regex_exarc))
        {
            // Get the index of the last member
            var regex_arc = /arc\s*\(\s*([^,()]+\s*,\s*[^,()]+)\s*\)\s*\./g;
            var match = true;
            var index = 0;
            while(match = regex_arc.exec(code)){index = match.index + match[0].length};

            // Append the new member to TextArea
            code = [code.slice(0,index), 'arc('+member1+','+member2+').', code.slice(index+1)].join('\n');

            textarea.value = code;
            // Updating TextArea by triggering the event "change"
            $("#"+idTextArea).trigger('change');
        }
    }
    
    selectTwo(idCanvas,addArc);   
}

var removeArc = function(idCanvas,idTextArea)
{   
    var deleteArc = function(member1,member2)
    {
        // Load TextArea
        var textarea = document.getElementById(idTextArea);
        
        // Delete arcs, top and bottom predicates
        var code = textarea.value;
        var regex_arc = new RegExp("arc\\s*\\(\\s*(("+member1+"\\s*,\\s*"+member2+")\|("+member2+"\\s*,\\s*"+member1+"))\\s*\\)\\s*\\.","g");
        code = code.replace(regex_arc,'\r');
        
        // Update Text and Graph
        textarea.value = code;
        $("#"+idTextArea).trigger('change');
    }
    
    selectTwo(idCanvas,deleteArc);
}

var selectTwo = function(idCanvas, func)
{
    // Load Canvas
    var canvas = document.getElementById(idCanvas);
    var ctx = canvas.getContext( "2d" );
   
    var select1 = null, select2 = null;

    // When user click on canvas
    $("#"+idCanvas).one("click",function(event){    
  
        // Get the first selected member
        select1 = detectSelectedMember(canvas,event); 
        
            // When user click on canvas
            $("#"+idCanvas).one("click",function(event){    

                // Get the second selected member
                select2 = detectSelectedMember(canvas,event);
                
                // Member selected
                if (select1 != null && select2 != null)
                    func(select1.member,select2.member);

                if (select1 == null)
                    alert('Member 1 not selected')
                
                if (select2 == null)
                    alert('Member 2 not selected')
                    
            });
    });
}

var normalize_from_text = function(idTextArea)
{
    $("#"+idTextArea).trigger('change');
}

var save_lattice = function(idTextArea, idTextBox)
{     
    // Get the text from TextArea and create a Blob object (file of plain data)
    var textToWrite = document.getElementById(idTextArea).value;
    var textBlob = new Blob([textToWrite], {type:'text/plain'});

    // Get name
    var fileName = document.getElementById(idTextBox).value;
    
    if (fileName == "")
    {
        alert("ERROR: The file must have a name");
        return;
    }
    
    // Create download link in DOM
    var downloadLink = document.createElement("a");
    downloadLink.download = fileName;
    
    // Allow save file in webkit and Gecko based browsers
    window.URL = window.URL || window.webkitURL;

    // Create the link Object
    downloadLink.href = window.URL.createObjectURL(textBlob);
    
    // Remove the link after the download using a function
    downloadLink.onclick = destroyClickedElement;
    document.body.appendChild(downloadLink);

    // Click the new link
    downloadLink.click();

    var destroyClickedElement = function(event)
    {
        // Remove the link from the DOM
        document.body.removeChild(event.target);
    }   
}

var upload_lattice_drop = function(idTextArea,idTextBox)
{
    var dropZone = document.getElementById(idTextArea); 
    
    dropZone.addEventListener('dragover', function(e) {
        e.stopPropagation();
        e.preventDefault();
        e.dataTransfer.dropEffect = 'copy';
    });

    // Get file data on drop
    dropZone.addEventListener('drop', function(evt) {
        evt.stopPropagation();
        evt.preventDefault();

        var files = evt.dataTransfer.files; // FileList object.
        var reader = new FileReader();  
        
        reader.onload = function(event) {            
             document.getElementById(idTextArea).value = event.target.result;
             $("#"+idTextArea).trigger('change');
        }     
        
        document.getElementById(idTextBox).value = files[0].name;
        reader.readAsText(files[0],"UTF-8"); 
    });
};

var upload_lattice_button = function(idTextArea,idTextBox,idUploadButton)
{
    var files = document.getElementById(idUploadButton).files;
    var reader = new FileReader();  
        
    reader.onload = function(event) {            
         document.getElementById(idTextArea).value = event.target.result;
         $("#"+idTextArea).trigger('change');
    }     
        
    document.getElementById(idTextBox).value = files[0].name;
    reader.readAsText(files[0],"UTF-8"); 
};



// PROPERTIES

var properties_tester = function(idTextarea,idCatCombo,idPropCombo,idAggrCombo1, idAggrCombo2, idMathText)
{   

    // Search all the connectives and, or and aggr in the textarea, calculates its arity and add them to the combobox
    // Second combobox is a copy of the first one
    var fill_connectives = function()
    {   
        $('#'+idAggrCombo1).empty();
        $('#'+idAggrCombo2).empty();
        
        var regex_con = /\W((?:and|or|aggr)_[^\s()]+)(\(.+\))?\s*:-/g;
        var match = true;

        while (match = regex_con.exec(textarea.value)) 
        { 
            var connective = document.createElement("option");

            var regex_arity = /[^,()]+/g;
            var arity = 0;

            while (match[2] != undefined && regex_arity.exec(match[2])) arity++;

            var name = match[1].replace('and','&');
            name = match[1].replace('or','|');
            name = match[1].replace('aggr','@');
            
            connective.text = name+'/'+arity.toString();
            connective.value = match[1];

            combo_con1.add(connective); 
        }
        
        $('#'+idAggrCombo1+' option').clone().appendTo('#'+idAggrCombo2);
    }

    // Depending on the category, the properties will be added to the property combobox
    var fill_properties = function() 
    {
        // Get selected category
        var category = combo_cat.options[combo_cat.selectedIndex].text;
        
        var prop_text = [];
        var prop_value = [];
        
        // Option.Text is the property name to display in the combobox
        // Option.value will be an array, first item is the predicate name in prolog, second one is the mathematical definition
        switch (category)
        {
            case 'Basic':
            {
                combo_con2.disabled = true;
                
                prop_value = ['frontier_top','frontier_bot','increasing',
                             'non_increasing','decreasing','non_decreasing',
                             'monotony','reflexivity','commutativity',
                             'associativity'];
                
                prop_text = ['Frontier top', 'Frontier bot', 'Increasing', 'Non increasing','Decreasing','Non decreasing','Monotony','Reflexivity','Commutativity','Associativity'];
                break;
            }
            
            case 'Combined':
            {
                combo_con2.disabled = true;
                
                prop_value = ['t_norm','t_conorm','implication'];
                prop_text = ['T-Norm', 'T-Conorm', 'Implication'];
                break;
            }
                
            case 'Multiple':
            {
                combo_con2.disabled = false;
                
                prop_def = ['$1($1(X, Y), Z) == $1(X, $1(Y, Z))', ];
                
                prop_value = ['switchness','distributivity','adjointness'];
                prop_text = ['Switchness', 'Distributitivy', 'Adjointness'];
                break;
            }
            
            default:
                break;
        }
        
        $('#'+idPropCombo).empty();
        
        // Add all the options
        for (var i = 0; i < prop_value.length; ++i)
        {
            var prop = document.createElement("option");
            prop.text = prop_text[i];
            prop.value = prop_value[i];
            combo_prop.add(prop);
        }
    }
    
    var writeMathDef = function()
    {
        // Get the math definition of the selected property
        var prop = combo_prop.options[combo_prop.selectedIndex].value;
        // Get the two connectives selected
        var conn1 = combo_con1.options[combo_con1.selectedIndex].value;
        var conn2 = combo_con2.options[combo_con2.selectedIndex].value;
        
        // Unicode characters
        var top = '\u22A4'; // T
        var bot = '\u22A5'; // Inverse T
        var lte = '\u2264'; // <=
        var gte = '\u2265'; // >=
        var prec = '\u227A'; // Preceds
        var impl = '\u2192'; // ->
        var iff = '\u21D4'; // <==>
        
        def_map = { 'frontier_top' : {param1 : '$1 ('+top+', '+top+') == '+ top},
                    'frontier_bot' : {param1 : '$1 ('+bot+', '+bot+') == '+ bot},
                    'increasing' : { param1 : 'If X '+prec+' Y '+impl+' $1(X, Z)  <  $1(Y, Z)', 
                                    param2 : 'If X '+prec+' Y '+impl+' $1(Z, X)  <  $1(Z, Y)'},
                    'non_increasing' : { param1 : 'If X '+prec+' Y '+impl+' $1(X, Z)  '+lte+'  $1(Y, Z)', 
                                        param2 : 'If X '+prec+' Y '+impl+' $1(Z, X)  '+lte+'  $1(Z, Y)'},
                    'decreasing' : { param1 : 'If X '+prec+' Y '+impl+' $1(X, Z)  >  $1(Y, Z)', 
                                    param2 : 'If X '+prec+' Y '+impl+' $1(Z, X)  >  $1(Z, Y)'},
                    'non_decreasing' : { param1 : 'If X '+prec+' Y '+impl+' $1(X, Z)  '+gte+'  $1(Y, Z)', 
                                        param2 : 'If X '+prec+' Y '+impl+' $1(Z, X)  '+gte+'  $1(Z, Y)'},
                    'monotony' : { param1 : 'If X '+prec+' Y '+impl+' $1(X, Z)  '+gte+'  $1(Y, Z)', 
                                  param2 : 'If X '+prec+' Y '+impl+' $1(Z, X)  '+gte+'  $1(Z, Y)'},
                    'reflexivity' : { param1 : '$1(X, X) == X'},
                    'commutativity' : { param1 : '$1(X, Y) == $1(Y, X)'},
                    'associativity' : { param1 : '$1( $1(X, Y), Z) == $1(X, $1(Y, Z))'},
                    'switchness' : { param1 : '$1( $2(X, Y), Z) == $2(X, $1(Y, Z))'},
                    'adjointness' : { param1 : '$1 is a T-Norm, $2 is a implication and X '+lte+' $2(Y,Z) '+iff+' $1(X,Z) '+lte+' Y'},
                    'distributivity' : { param1 : '$2( $1(X, Y), Z) == $1( $2(X, Z), $2(Y, Z))', param2 : '$2(X,$1(Y, Z)) == $1( $2(X, Y), $2(X, Z))'},
                    't_norm' : { param1 : 'Top is the identity element, $1 is commutative, associative and monotone'},
                    't_conorm' : { param1 : 'Bot is the identity element, $1 is commutative, associative and monotone'},
                    'implication' : { param1 : 'If X '+prec+' Y '+impl+' $1(X, Z)  '+lte+'  $1(Y, Z)',
                                    param2 : 'If X '+prec+' Y '+impl+' $1(Z, X)  '+gte+'  $1(Z, Y)'}
                  };
        
        var param1 = def_map[prop].param1;
        var param2 = def_map[prop].param2;
        
        // Replace $1 for the name of the connective (if there is any selected)
        param1 = param1.replace(/\$1/g,conn1);
        param1 = param1.replace(/\$2/g,conn2);
        
        if (param2)
        {
            param2 = param2.replace(/\$1/g,conn1);
            param2 = param2.replace(/\$2/g,conn2);
            math_text.value = 'Param1:\t'+param1+'\nParam2:\t'+param2;
        }
        else
            math_text.value = param1;
    }
    
    var textarea = document.getElementById(idTextarea);
    var combo_cat = document.getElementById(idCatCombo);
    var combo_prop = document.getElementById(idPropCombo);
    var combo_con1 = document.getElementById(idAggrCombo1);
    var combo_con2 = document.getElementById(idAggrCombo2);
    var math_text = document.getElementById(idMathText);

    jQuery('#'+idTextarea).bind('change',fill_connectives);
    jQuery('#'+idCatCombo).bind('change',function(){ fill_properties(); writeMathDef() });
    jQuery('#'+idPropCombo).bind('change',writeMathDef);
    
    $('#'+idTextarea).trigger('change');
    $('#'+idCatCombo).trigger('change');
};