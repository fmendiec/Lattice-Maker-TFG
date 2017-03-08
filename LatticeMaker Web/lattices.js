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
			if( from.member == hover ) { tmp_radius = radius_hover; }
			var alpha = Math.atan( Math.abs( from.y - to.y ) / Math.abs( from.x - to.x ) );
			var x = tmp_radius * Math.cos(alpha);
			var y = tmp_radius * Math.sin(alpha);
			if( from.x > to.x ) { x = -x; }
			if( from.y > to.y ) { y = -y; }
			ctx.lineWidth = 2;
			ctx.fillStyle = "#000000";
			ctx.strokeStyle = "#000000";
			ctx.beginPath();
			ctx.arc( from.x + x, from.y + y, 5, 0, 2 * Math.PI );
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
	var members = [];
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
	jtextarea.bind( "change", updateByText );
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