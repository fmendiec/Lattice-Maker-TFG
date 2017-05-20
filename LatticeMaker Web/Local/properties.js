var properties_tester = function(idTextarea,idCatCombo,idPropCombo,idAggrCombo1, idAggrCombo2)
{   
    var fill_connectives = function()
    {
        var regex_and = /\s*and_([^\s()]+)(\(.+\))?\s*:-/g;
        var regex_or = /\s*or_([^\s()]+)(\(.+\))?\s*:-/g;
        var regex_aggr = /\s*aggr_([^\s()]+)(\(.+\))?\s*:-/g;

        $('#'+idAggrCombo1).empty();
        $('#'+idAggrCombo2).empty();
        addConnective(regex_and,'&');
        addConnective(regex_or,'|');
        addConnective(regex_aggr,'@');

        $('#'+idAggrCombo1+' option').clone().appendTo('#'+idAggrCombo2);
    }

    var addConnective = function(regex,symbol)
    {   
        var regex_con = regex;
        var match = true;

        while (match = regex_con.exec(textarea.value)) 
        { 
            var connective = document.createElement("option");

            var regex_arity = /[^,()]+/g;
            var arity = 0;

            while (match[2] != undefined && regex_arity.exec(match[2])) arity++;

            connective.text = symbol+'_'+match[1]+'/'+arity.toString();

            combo_con1.add(connective); 
        }
    }

    var fill_properties = function() 
    {
        var category = combo_cat.options[combo_cat.selectedIndex].text;
        var prop_text = [];
        var prop_value = [];
        
        switch (category)
        {
            case 'Basic':
            {
                combo_con2.disabled = true;
                prop_value = ['frontier_top','frontier_bot','increasing','non_increasing','decreasing','non_decreasing','monotony','reflexivity','commutativity','associativity'];
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
                prop_value = ['switchness','distributivity','adjointness'];
                prop_text = ['Switchness', 'Distributitivy', 'Adjointness'];
                break;
            }
            
            default:
                break;
        }
        
        $('#'+idPropCombo).empty();
        
        for (var i = 0; i < prop_value.length; ++i)
        {
            var prop = document.createElement("option");
            prop.text = prop_text[i];
            prop.value = prop_value[i];
            combo_prop.add(prop);
        }
        
    }
    
    var textarea = document.getElementById(idTextarea);
    var combo_cat = document.getElementById(idCatCombo);
    var combo_prop = document.getElementById(idPropCombo);
    var combo_con1 = document.getElementById(idAggrCombo1);
    var combo_con2 = document.getElementById(idAggrCombo2);

    jQuery('#'+idTextarea).bind('change',fill_connectives);
    jQuery('#'+idCatCombo).bind('change',fill_properties);
    
    $('#'+idTextarea).trigger('change');
    $('#'+idCatCombo).trigger('change');
}