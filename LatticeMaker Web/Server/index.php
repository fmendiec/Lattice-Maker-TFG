<?php

// Requires
require( 'content/php/req/functions.php' );
require( 'content/php/req/mysql.php' );

// Página
$_view = getURI( 'view', 'home' );
if ( substr( $_view, -1 ) == '/' ) { $_view = substr( $_view, 0, strlen( $_view ) - 1 ); }
if ( !file_exists( "content/php/inc/$_view.php" )) {
	$_view = '404';
}

// Si es un predicado
if ( $_view == 'predicate' ) {
	$predicate = explode( '-', getURI( 'predicate', ' - ' ) );
	if ( count( $predicate ) == 2 ) {
		$name = $predicate[0];
		$arity = intval( $predicate[1] );
		$query = mysql_query( "SELECT * FROM predicate WHERE name = '$name' AND arity = $arity", $_mysql );
		if ( mysql_num_rows( $query ) > 0 ) {
			$_predicate = mysql_fetch_assoc( $query );
			mysql_free_result( $query );
		} else {
			$_view = "404";
		}
	} else {
		$_view = "404";
	}
}

// Error 404
if ( $_view == '404' ) {
	header( 'HTTP/1.0 404 Not Found' );
}

// Metadatos
include( "content/php/inc/meta/default.php" );
include( "content/php/inc/meta/$_view.php" );

?>
<!DOCTYPE html>
<html lang="en">
	<head>
		<title>FLOPER: <?php echo $_title; ?></title>
		<base href="http://dectau.uclm.es/unfold/" target="_self">
		<meta name="description" content="<?php echo $_description; ?>" />
		<meta name="author" content="José Antonio Riaza Valverde" />
		<meta charset="UTF-8" />
		<script type="text/javascript" src="content/js/jQuery.js"></script>
		<script type="text/javascript" src="content/js/main.js"></script>
		<script type="text/javascript" src="content/js/highlight.js"></script>
		<script type="text/javascript" src="content/js/tree.js"></script>
		<script type="text/javascript" src="content/js/lattices.js"></script>
        <script type="text/javascript" src="content/js/buttons.js"> </script>
        <script type="text/javascript" src="content/js/properties.js"> </script>
		<script>hljs.initHighlightingOnLoad();</script>
		<link href="content/img/favicon.ico?d=30082016" type="image/x-icon" rel="icon" />
		<link href='https://fonts.googleapis.com/css?family=Open+Sans:400,700,300,400italic,700italic' rel='stylesheet' type='text/css'>
		<link href='https://fonts.googleapis.com/css?family=Roboto+Mono' rel='stylesheet' type='text/css'>
		<link rel="StyleSheet" href="content/css/main.css" type="text/css" media="ALL" />
        <link rel="StyleSheet" href="content/lattices.css" type="text/css" media="ALL">
		<link rel="StyleSheet" href="content/css/highlight.css" type="text/css" media="ALL" />
	</head>
	<body>
		<!-- NAV -->
		<div id="nav">
			<div class="match-width">
				<?php if( $_view != 'home' ) { ?> 
				<a href="./" id="nav-logo"></a>
				<?php } ?>
				<form id="search" action="search" method="get">
					<input type="text" name="search" placeholder="Search..." title="Search" />
				</form>
				<ul>
					<li><a <?php if( $_view == 'try' ) { echo 'class="nav-selected"'; } ?> href="try" title="Try FLOPER online">Try it online</a></li>
					<li><a <?php if( $_view == 'downloads' ) { echo 'class="nav-selected"'; } ?> href="downloads" title="Downloads">Downloads</a></li>
					<li><a <?php if( $_view == 'documentation' ) { echo 'class="nav-selected"'; } ?> href="documentation" title="Documentation">Documentation</a></li>
				</ul>
			</div>
		</div>
		<div id="result">
			<div></div>
		</div>

<?php if( $_view != 'home' ) { ?>
		<!-- HEADER -->
		<div class="match-width triangles triangles-empty"></div>
<?php } ?>
		<!-- CONTENT -->
		<div id="content"><?php include( "content/php/inc/$_view.php" ); ?></div>

		<!-- FOOTER -->
		<div id="footer">
			<div class="match-width">University of Castilla-La Mancha, <a href="http://dectau.uclm.es/" target="_blank">DEC-Tau</a></div>
		</div>
	</body>
</html>
<?php mysql_close( $_mysql ); ?>
