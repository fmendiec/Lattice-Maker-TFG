/**
 * FLOPER DERIVATION TREE
 * 
 * The derivation tree is read in the
 * following format (four spaces to
 * denote child nodes) and is drawn:
 * 
 * Rule0 < Result0, Unifier0 >
 *     Rule1 < Result1, Unifier1 >
 *         Rule2 < Result2, Unifier2 >
 *         Rule3 < Result3, Unifier3 >
 *     Rule4 < Result4, Unifier4 >
 *         Rule5 < Result5, Unifier5 >
 * 
 * How to use: $floper(src, dst) where
 *     src = <textarea> id containing the tree in text format
 *     dst = <canvas> id on which to paint the tree
 * 
 * Example:
 *     <textarea id="tree-txt">...</textarea>
 *     <canvas id="tree-img"></canvas>
 *     <script type="text/javascript" src="floper.min.js"></script>
 *     <script type="text/javascript">$floper("tree-txt","tree-img");</script>
 *
 * Author: José Antonio Riaza Valverde
 * Date: 2016-08-14
 */

// Rectángulos con borde redondeado
CanvasRenderingContext2D.prototype.roundRect = function(sx,sy,ex,ey,r) {
	var r2d = Math.PI/180;
	ex = ex + sx;
	ey = ey + sy;
	this.beginPath();
	this.moveTo(sx+r,sy);
	this.lineTo(ex-r,sy);
	this.arc(ex-r,sy+r,r,r2d*270,r2d*360,false);
	this.lineTo(ex,ey-r);
	this.arc(ex-r,ey-r,r,r2d*0,r2d*90,false);
	this.lineTo(sx+r,ey);
	this.arc(sx+r,ey-r,r,r2d*90,r2d*180,false);
	this.lineTo(sx,sy+r);
	this.arc(sx+r,sy+r,r,r2d*180,r2d*270,false);
	this.closePath();
}

// FLOPER
var Floper = {
	
	// Dibujar árbol
	init: function(src, canvas) {
		// Configuración del grafo
		var padding = 5;
		var margin_x = 10;
		var margin_y = 20;
		var radius = 18;
		var font_size = 14;
		// Obtener contenedor origen
		var src = document.getElementById(src);
		// Obtener lienzo destino
		var canvas = document.getElementById(canvas);
		// Obtener contexto
		var ctx = canvas.getContext("2d");
		// Crear árbol desde el texto
		var tree = $tree(src.value);
		// Calcular ancho máximo de cada nivel
		var maxwidth = tree.map(function(a){return a.reduce(function(b,c){return b + c.width(ctx, margin_x, padding)}, 0)}, 0);
		// Calcular ancho de la imagen
		var width = maxwidth.reduce(function(a,b){return Math.max(a,b)}, 0);
		// Calcular altura de un bloque de unificación
		var unifier_height = (padding * 3) + ((font_size + 2) * 2);
		// Calcular altura de un nivel
		var level_height = (radius * 2) + (margin_y * 2) + unifier_height;
		// Calcular altura de la imagen
		var height = level_height * tree.length - (2 * radius);
		// Propiedades del lienzo
		ctx.height = height;
		ctx.width = width;
		canvas.height = ctx.height;
		canvas.width = ctx.width;
		// Array para almacenar centros relativos de otros niveles
		var relative = [];
		// Altura
		var offset_y = margin_y;
		var offset_z = margin_y;
		// Iterar niveles
		for (var i = 0; i < tree.length; i++) {
			// Insertar centros para niveles posteriores
			relative.push([]);
			// Calcular offset
			var offset_x = (width - maxwidth[i]) / 2;
			// Iterar bloques
			for (var j = 0; j < tree[i].length; j++) {
				// Recuperar offset inicial
				offset_z = offset_y;
				// Calcular centro relativo
				var center = offset_x + (tree[i][j].width(ctx, margin_x, padding) / 2);
				// Almacenar centro relativo
				relative[i].push(center);
				if (i > 0) {
					// Pintar líneas
						ctx.lineWidth = 2;
						ctx.strokeStyle = "#222222";
						ctx.beginPath();
						ctx.moveTo(relative[i-1][tree[i][j].parent], offset_y - unifier_height / 2);
						ctx.lineTo(center, offset_y + 2 * (margin_y + radius));
						ctx.closePath();
						ctx.stroke();
					// Pintar regla
					offset_z += 2 * (margin_y + radius)
				}
				offset_z += unifier_height;
				// Actualizar offset
				offset_x += tree[i][j].width(ctx, margin_x, padding);
			}
			// Actualizar offset
			offset_y = offset_z;
		}
		// Array para almacenar centros relativos de otros niveles
		relative = [];
		// Altura
		offset_y = margin_y;
		offset_z = margin_y;
		// Iterar niveles
		for (i = 0; i < tree.length; i++) {
			// Insertar centros para niveles posteriores
			relative.push([]);
			// Calcular offset
			offset_x = (width - maxwidth[i]) / 2;
			// Iterar bloques
			for (j = 0; j < tree[i].length; j++) {
				// Recuperar offset inicial
				offset_z = offset_y;
				// Calcular centro relativo
				center = offset_x + (tree[i][j].width(ctx, margin_x, padding) / 2);
				// Almacenar centro relativo
				relative[i].push(center);
				if (i > 0) {
					var x1 = Math.abs(relative[i-1][tree[i][j].parent] - center);
					var y1 = 2 * (radius + margin_y) + unifier_height / 2;
					var y3 = margin_y + radius;
					var alpha = Math.atan(y1 / x1);
					var y4 = y3 + radius * Math.sin(alpha);
					var x3 = y3 / (Math.tan(alpha));
					var x4 = x3 - radius * Math.cos(alpha);
					if (relative[i-1][tree[i][j].parent] < center) {
						x3 = -x3;
						x4 = -x4;
					}
					// Pintar nexo
					Floper.drawNexus(ctx);
						ctx.beginPath();
						ctx.arc(center + x4, offset_y + y4, 7, 0, 2 * Math.PI);
						ctx.closePath();
						ctx.fill();
					// Pintar regla
					offset_z += margin_y + radius;
						Floper.drawRule(ctx);
							ctx.lineWidth = 4;
							ctx.beginPath();
							ctx.arc(center + x3, offset_z, radius, 0, 2 * Math.PI);
							ctx.closePath();
							ctx.stroke();
							ctx.fill();
						Floper.drawText(ctx);
						ctx.font = "bold 14px Roboto Mono, Monospace, Courier New";
						ctx.fillStyle = "#222222";
							ctx.fillText(tree[i][j].rule, center + x3, offset_z + font_size / 2 - 1);
					offset_z += radius + margin_y;
				}
				// Pintar unificador
					Floper.drawUnifier(ctx);
						ctx.lineWidth = 4;
						ctx.strokeStyle = "#222222";
						ctx.roundRect(center - tree[i][j].width(ctx, 0, padding) / 2, offset_z, tree[i][j].width(ctx, 0, padding), unifier_height, 10);
						ctx.stroke();
						ctx.fill();
					Floper.drawText(ctx);
						ctx.fillText(tree[i][j].result, center, offset_z + padding + font_size);
						ctx.fillText(tree[i][j].unifier, center, offset_z + (2 * font_size) + (2 * padding));
				offset_z += unifier_height;
				// Actualizar offset
				offset_x += tree[i][j].width(ctx, margin_x, padding);
			}
			// Actualizar offset
			offset_y = offset_z;
		}
		// Añadir enlace
		if (canvas.addEventListener) {
			canvas.addEventListener("click", function(){ window.open(canvas.toDataURL(), '_blank'); }, false);
			canvas.style.cursor = "pointer";
		} else {
			if (canvas.attachEvent) {
				canvas.attachEvent("click", function(){ window.open(canvas.toDataURL(), '_blank'); });
				canvas.style.cursor = "pointer";
			}
		}
	},
	
	// Preparar contexto para pintar regla
	drawRule: function(ctx) {
		ctx.fillStyle = "#7e7fff"; // #7e7fff
		ctx.strokeStyle = "#222222";
	},
	
	// Preparar contexto para pintar unificador
	drawUnifier: function(ctx) {
		ctx.fillStyle = "#ffff00"; // #ffff00
		ctx.strokeStyle = "#222222";
	},
	
	// Preparar contexto para pintar texto
	drawText: function(ctx) {
		ctx.fillStyle = "#222222"; // #222222
		ctx.font = "14px Roboto Mono, Monospace, Courier New";
		ctx.textAlign = "center"; 
	},
	
	// Preparar contexto para medir ancho
	measureText: function(ctx) {
		ctx.font = "14px Roboto Mono, Monospace, Courier New";
		ctx.textAlign = "center"; 
	},
	
	// Preparar contexto para pintar nexo
	drawNexus: function(ctx) {
		ctx.fillStyle = "#222222";
	},
	
	// Pintar el lienzo blanco
	drawBackground: function(ctx, width, height) {
		ctx.fillStyle = "#ffffff";
		ctx.fillRect(0, 0, width, height);
	}
	
}

// ESTRUCTURA DE ÁRBOL
var Tree = {
	
	// Inicializar árbol desde una cadena
	init: function(str) {
		// Eliminar espacios innecesarios
		str = str.replace(/\n\n/g, "");
		// Separar en líneas
		var lines = str.split("\n");
		// Primera línea útil
		var j = 0;
		while (j < lines.length && lines[j].indexOf(",") === -1) {
			j++;
		}
		// Crear árbol con la raíz
		var tree = [[new Tree.node(lines[j], 0, 0)]];
		// Iterar líneas
		for (var i = j + 1; i < lines.length; i++) {
			// Si es una línea útil
			if (lines[i].indexOf(",") != -1) {
				// Calcular profundidad
				var deep = (lines[i].match(/    /g) || []).length;
				// Calcular padre
				var parent = tree[deep - 1].length - 1
				// Crear array si no existe
				if (tree.length == deep) { tree.push([]); }
				// Añadir hijo
				tree[deep].push(new Tree.node(lines[i], deep, parent));
			}
		}
		// Devolver árbol
		return tree;
	},
	
	// Estructura del nodo
	node: function(str, deep, parent) {
		// Profundidad del nodo
		this.deep = deep;
		// Padre del nodo
		this.parent = parent;
		// Regla disparada
		this.rule = str.split(" <")[0].replace(/ /g, "");
		// Resultado
		this.result = str.split(" <")[1].split(", ")[0].replace(/ /g, "");
		this.result = this.result.length > 100 ? this.result.substring(0, 100) + "..." : this.result;
		// Unificador
		this.unifier = str.split(" <")[1].split(", ")[1].replace("} >", "}").replace(/ /g, "");
		this.unifier = this.unifier.length > 100 ? this.unifier.substring(0, 100) + "..." : this.unifier;
		// Tamaño del bloque canvas
		this.width = function(ctx, margin, padding) {
			Floper.measureText(ctx);
			return Math.max(ctx.measureText(this.result).width, ctx.measureText(this.unifier).width) + (2 * margin) + (2 * padding);
		};
	}
	
};

// ALIAS
// $tree(str) = Tree.init(str)
$tree = Tree.init;
// $floper(src, canvas) = Floper.init(src, canvas)
$floper = Floper.init;

