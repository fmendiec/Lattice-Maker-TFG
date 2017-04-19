var addMember = function(idTextArea)
{
    
    // Load TextArea
    var textarea = document.getElementById(idTextArea);
    
    // Read the new member's name
    var newMember = prompt("What is the name of the new member?");
    
    // Cancel
    if (newMember == null)
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
        var code = $("#"+idTextArea).val();
        var regex_member = /member\s*\(\s*(.+)\s*\)\s*\./g;
        var match = true;
        var index = 0;
        while(match = regex_member.exec(code)){index = match.index + match[0].length};

        var member = 'member('+newMember+').'

        // Append the new member to TextArea
        textarea.value = [textarea.value.slice(0,index), member, textarea.value.slice(index+1)].join('\n');

        // Get the index of the members list
        code = $("#"+idTextArea).val();
        var regex_members = /\[\s*(.+)\s*\]\s*\)\s*\./g;
        match = regex_members.exec(code);

        // Create Members section
        if (match == null)
        {
            index += member.length;

            textarea.value = [textarea.value.slice(0,index+1), 'members(['+newMember+']).', textarea.value.slice(index+1)].join('\n'); 
        }

        // Add to members section
        else
        {
            index = match.index + match[1].length + 1;

            // Append the new member to the list in TextArea
            textarea.value = [textarea.value.slice(0,index), ','+newMember, textarea.value.slice(index)].join('');
        }

        // Updating TextArea by triggering the event "change"
        $("#"+idTextArea).trigger('change');
    }
}

var removeMember = function(idCanvas,idTextArea)
{    
    // Load Canvas
    var canvas = document.getElementById(idCanvas);
   
    var selected = null;
    
    $("#"+idCanvas).css("cursor", "crosshair");

    // When user click on canvas
    $("#"+idCanvas).one("click",function(event){    
        
        // Get selected member
        selected = detectSelectedMember(canvas,event);    
        
        // Member selected
        if (selected != null)
            deleteMember(idTextArea,selected.member);
        
        else
            alert('Member not selected') 
    });
    
    // Delete member predicates
    var deleteMember = function(idTextArea,member) 
    {
         // Load TextArea
        var textarea = document.getElementById(idTextArea);
        var code = $("#"+idTextArea).val();
        
        // Delete member predicates
        var regex_member = new RegExp("member\\s*\\(\\s*"+member+"\\s*\\)\\s*\\.","g");
        code = code.replace(regex_member,'\r');
        
        // Delete arcs, top and bottom predicates
        var regex_arc = new RegExp("arc\\s*\\(\\s*("+member+",.+)|(.+,"+member+")\\s*\\)\\s*\\.","g");
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
        var code = $("#"+idTextArea).val();
        
        // Arc formed by the two members
        var regex_exarc = new RegExp("arc\\s*\\(\\s*(("+member1+"\\s*,\\s*"+member2+")\|("+member2+"\\s*,\\s*"+member1+"))\\s*\\)\\s*\\.","g");
        
        // If the arc exists, don't write it
        if (code.match(regex_exarc) == null)
        {
            // Get the index of the last member
            var regex_arc = /arc\s*\(\s*([^,()]+\s*,\s*[^,()]+)\s*\)\s*\./g;
            var match = true;
            var index = 0;
            while(match = regex_arc.exec(code)){index = match.index + match[0].length};

            // Append the new member to TextArea
            textarea.value = [textarea.value.slice(0,index), 'arc('+member1+','+member2+').', textarea.value.slice(index+1)].join('\n');

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
        var code = $("#"+idTextArea).val();
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

var load_lattice = function(idTextArea,idTextBox)
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
        
        console.log(files);
        document.getElementById(idTextBox).value = files[0].name;
        reader.readAsText(files[0],"UTF-8"); 
    });
}