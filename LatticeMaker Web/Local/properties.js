var getAggrFromTextArea = function(idTextarea, idAggrCombo1, idAggrCombo2)
{    
    var addAggr = function()
    {
        $('#'+idAggrCombo1).empty();
        
        var and_aggr = /\s*and_(.+):-/g;
        var match = true;
        
        while (match = and_aggr.exec(textarea.value)) 
        { 
            console.log(match)
            var aggr = document.createElement("option");
            var regex_name_arity = /(.+)\s*(\(.*\)|\s*:-)/g;

            var name_arity = regex_name_arity.exec(match[1]);
            
            console.log(name_arity);

            aggr.text = "&_"+name_arity[1];

            combo1.add(aggr);           
        }
    }
    
    var textarea = document.getElementById(idTextarea);
    var combo1 = document.getElementById(idAggrCombo1);
    var combo2 = document.getElementById(idAggrCombo2);
    
    jQuery('#'+idTextarea).bind('change',addAggr);
    
    $('#'+idTextarea).trigger('change');
}