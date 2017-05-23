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
                prop_text = ['T-Norm', 'T-Conorm', 'Implicaiton'];
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
                    'switchness' : { param1 : '$1( $2(X, Y), Z) == $2(X, $1(Y, Z))'}
                  };
        
        
        def = def_map[prop].param1;
        
        // Replace $1 for the name of the connective (if there is any selected)
        def = def.replace(/\$1/g,conn1);
        
        math_text.value = def;
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
}