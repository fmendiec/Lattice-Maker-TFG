// Variables previas
var run = false;
var tuning = false;
var transformation_current_id = 0;
var transformations = [];

// Resaltar referencia
function highlightref(id) {
	jQuery('.highlightref').removeClass('highlightref');
	jQuery(id).addClass('highlightref');
}

// Mostrar desplegado
function show_unfold() {
	jQuery('#unfold-status').html('Parsing rules...');
	jQuery('.unfold-rules').html('<div class="loading"></div>');
	jQuery('#unfold-visible').stop().slideDown(300, function(){
		jQuery('#unfold-visible').css('height', 'auto');
	});
	jQuery('#unfold-state').html('-');
	jQuery('#unfold-toggle').attr('title','Hide rules to unfold');
	jQuery.ajax({
		url: "content/pl/try-rules.php",
		method: "post",
		data: jQuery('#try-form').serialize(),
		dataType: "html",
		success: function(data, textStatus, jqXHR) {
			jQuery('#unfold-status').html('Select the rule to unfold:');
			jQuery('.unfold-rules').html(data);
			jQuery('.unfold-rules code').each(function(i, block) {
				hljs.highlightBlock(block);
			});
		}
	});
}

// Ocultar desplegado
function hide_unfold() {
	jQuery('#unfold-visible').stop().slideUp(300);
	jQuery('#unfold-state').html('+');
	jQuery('#unfold-toggle').attr('title','Show rules to unfold');
}

// Mostrar/Ocultar desplegado
function toggle_unfold() {
	if (jQuery('#unfold-visible').is(':visible')) {
		hide_unfold();
	} else {
		show_unfold();
	}
}

// Cargar transformación anterior
function transformation_prev() {
	if (transformation_current_id > 0) {
		transformation_current_id -= 1;
		jQuery('#try-fpl').val(transformations[transformation_current_id]);
	}
	transformation_update();
}

// Cargar transformación posterior
function transformation_next() {
	if (transformation_current_id < transformations.length - 1) {
		transformation_current_id += 1;
		jQuery('#try-fpl').val(transformations[transformation_current_id]);
	}
	transformation_update();
}

// Reiniciar desplegado
function transformation_reset() {
	transformation_current_id = 0;
	transformations = [];
	transformation_update();
}

// Actualizar enlaces de desplegado
function transformation_update() {
	if (transformation_current_id > 0) {
		jQuery('#unfold-prev').removeClass('link-disabled');
	} else {
		jQuery('#unfold-prev').addClass('link-disabled');
	}
	if (transformation_current_id < transformations.length - 1) {
		jQuery('#unfold-next').removeClass('link-disabled');
	} else {
		jQuery('#unfold-next').addClass('link-disabled');
	}
	jQuery('#unfold-current').html(transformation_current_id);
	hide_unfold();
}

// Desplegar regla
function unfold(n) {
	jQuery('#unfold-status').html('Unfolding...');
	jQuery('.unfold-rules').html('<div class="loading"></div>');
	jQuery.ajax({
		url: "content/pl/try-unfold.php",
		method: "post",
		data: jQuery('#try-form').serialize() + "&rule=" + n,
		dataType: "text",
		success: function(data, textStatus, jqXHR) {
			hide_unfold();
			if (transformations.length == 0) {
				transformations.push(jQuery('#try-fpl').val());
			}
			transformation_current_id = transformations.length;
			transformations.push(data);
			jQuery('#try-fpl').val(data);
			transformation_update();
		}
	});
}

// Aplicar substitución
function apply_substitution(subs) {
	jQuery.ajax({
		url: "content/pl/try-substitution.php",
		method: "post",
		data: jQuery('#try-form').serialize() + '&subs=' + subs,
		dataType: "text",
		success: function(data, textStatus, jqXHR) {
			hide_unfold();
			if (transformations.length == 0) {
				transformations.push(jQuery('#try-fpl').val());
			}
			jQuery('#try-fpl').val(data);
			transformation_reset()
		}
	});
}

// Documento leído
jQuery(document).ready(function(){

	// Buscador
	setTimeout(function(){ jQuery('#result').css('left',jQuery('#search').position().left); }, 1000);
	jQuery(window).bind('resize', function(){
		jQuery('#result').css('left',jQuery('#search').position().left);
	});
	jQuery('#search > input').bind('change textInput input focus', function(){
		if (typeof(searchOut) != "undefined") { clearTimeout(searchOut); };
		search = jQuery('#search > input').val();
		searchOut = setTimeout(function(){
			if (search.length > 0) {
				jQuery.ajax({
					url: "content/js/search.php",
					data: "search=" + search,
					dataType: "html",
					success: function(data, textStatus, jqXHR) {
						jQuery('#result > div').html(data);
						jQuery('#result').show();
					}
				});
			} else {
				jQuery('#result').hide();
			}
		}, 500);
	});
	jQuery('body').bind('click', function(){
		if (!jQuery('#search > input').is(':focus')) {
			if (typeof(searchOut) != "undefined") { clearTimeout(searchOut); };
			jQuery('#result').hide();
		}
	});

	// Logo
	jQuery('#nav-logo').mouseover(function(){
		jQuery(this).stop().fadeTo(300, 1.0);
	}).mouseout(function(){
		jQuery(this).stop().fadeTo(300, 0.5);
	});

	// Editor online
	jQuery('.code-try').bind('focus', function(){
		jQuery(this).stop().css('border', '0px solid #666666').animate({'border-left-width': 10, 'border-radius': 0}, 200);
	}).bind('blur', function(){
		jQuery(this).stop().animate({'border-left-width': 0, 'border-radius': 10}, 200);
	});
	jQuery('#try-fpl, #try-lat, #try-sim').bind('change textInput input', function(){
		hide_unfold();
		transformation_reset();
	});

	// Cargar ficheros
	jQuery('.button-try-reset').bind('click', function(){
		var file = jQuery(this).val();
		var ext = file.substr(-4) == "test" ? "test" : file.substr(-3).replace('.pl', 'lat');
		jQuery.ajax({
			url: "content/txt/" + file + ".txt",
			dataType: "text",
			success: function(data, textStatus, jqXHR) {
				jQuery('#try-' + ext).val(data);
				hide_unfold();
				transformation_reset();
			}
		});
	});
	
	// Ejecución asíncrona
	jQuery('#try-form').bind('submit', function(e){
		e.preventDefault();
		if (!run) {
			run = true;
			jQuery('#button-try-run').addClass('button-try-run-disabled').val('Generating');
			jQuery('#try-answers').html('<div class="loading"></div>');
			jQuery('#try-tree').html('<canvas id="derivation-tree-img"></canvas>');
			jQuery('#answers').stop().slideDown(300, function(){
				jQuery('html, body').animate({scrollTop: jQuery('#answers').offset().top}, 500);
			});
			jQuery.ajax({
				url: "content/pl/try-run.php",
				method: "post",
				data: jQuery('#try-form').serialize(),
				dataType: "html",
				success: function(data, textStatus, jqXHR) {
					jQuery('#try-answers').html(data);
					jQuery('#try-answers code').each(function(i, block) {
						hljs.highlightBlock(block);
					});
					jQuery('#try-tree').prepend('<img src="content/img/ajax-loader.gif" alt="loading" class="loading" />');
					if (jQuery('#derivation-tree-txt').length > 0) {
						$floper('derivation-tree-txt', 'derivation-tree-img');
						setTimeout(function(){
							jQuery('html, body').animate({scrollTop: jQuery('#answers').offset().top}, 500);
						}, 200);
					} else {
						jQuery('#try-answers').append('<p>There was an error.</p>');
					}
					jQuery('#try-tree img').remove();
					run = false;
					jQuery('#button-try-run').removeClass('button-try-run-disabled').val('Generate');
					jQuery('#try-tree').animate({scrollLeft: jQuery('#try-tree canvas').width() / 2 - jQuery('#try-tree').width() / 2}, 500);
					jQuery('#derivation-tree-code code').html(jQuery('#derivation-tree-txt').val());
					jQuery('#derivation-tree-code code').each(function(i, block) {
						hljs.highlightBlock(block);
					});
				}
			});
		}
	});
	
	// Pestañas
	jQuery('.tab').bind('click',function(){
		if (!jQuery('#content-' + jQuery(this).attr('id')).is(':visible')) {
			jQuery('.content-tab').stop().slideUp();
			jQuery('#content-' + jQuery(this).attr('id')).stop().slideDown();
			jQuery('.tab').removeClass('tab-visible');
			jQuery(this).addClass('tab-visible');
		}
	});
	
	// Tuning
	jQuery('#button-try-tuning').bind('click', function(e){
		e.preventDefault();
		if (!tuning) {
			tuning = true;
			jQuery('#tuning-sub').html('<div class="loading"></div>');
			jQuery('#button-try-tuning').addClass('button-try-tuning-disabled').val('Generating substitution');
			jQuery.ajax({
				url: "content/pl/try-tuning.php",
				method: "post",
				data: jQuery('#try-form').serialize(),
				dataType: "html",
				success: function(data, textStatus, jqXHR) {
					jQuery('#tuning-sub').html(data);
					jQuery('#tuning-sub code').each(function(i, block) {
						hljs.highlightBlock(block);
					});
					tuning = false;
					jQuery('#button-try-tuning').removeClass('button-try-tuning-disabled').val('Generate substitution');
					jQuery('#button-try-replace').removeClass('button-subs-disabled').addClass('button-class-enabled').unbind('click').bind('click', function(){
						apply_substitution(jQuery('.apply_substitution:last').text());
						jQuery('html, body').animate({scrollTop: 0}, 500);
					});
				}
			});
		}
	});

})
