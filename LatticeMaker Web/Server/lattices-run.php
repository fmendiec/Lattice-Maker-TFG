<?php

error_reporting(0);

// Recoger datos
$lat = $_POST['lat'];
$aggr1 = $_POST['aggr1'];
$aggr2 = $_POST['aggr2'];
$prop = $_POST['prop'];


// Número aleatorio
$ran = rand(1000, 9999);

// Escribir ficheros
$file_lat = fopen( "$ran-lattice.pl", 'w' );
fwrite( $file_lat, $lat );
fclose( $file_lat );

try {
	
	// Ejecutar comando
    if (!$aggr2)
	   $cmd = "nice -n15 pl -f LatticeTest.pl -g \"test_property('$ran-lattice.pl',$prop,$aggr1),halt\"";
    else
        $cmd = "nice -n15 pl -f LatticeTest.pl -g \"test_property('$ran-lattice.pl',$prop,$aggr1,$aggr2),halt\"";
    
	// Imprimir resultado
	echo shell_exec( $cmd );

} catch (Exception $e) {
	
	// Mensaje de error
    echo "<div class=\"error-message\">There was an error.</div>";
    
}

// Eliminar ficheros
unlink( "$ran-lat.pl" );
	
?>