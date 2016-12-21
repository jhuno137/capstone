$(function() {
    
    function htmlEntities(str) {
        return String(str).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
    }
    
    // when the send button is clicked clear the text input
    $("#sendButton").on('click',function(){
        $("#userInput").val('');
    });
    // when one of the predicted words is clicked, append it to the user input
    $("#predOutput1,#predOutput2,#predOutput3").on('click',function(e){
        e.preventDefault();
        current = $("#userInput").val();
        txt = current.trim() + " " + $(this).text().trim() + " ";
        $("#userInput").val(txt);
        // register the event
        Shiny.onInputChange("userInput",txt);
        // return the focus to the text input
        $("#userInput").focus();
    });
    // append input text to "screen"" output when enter is pressed
    $('#userInput').keypress(function (e) {
      if (e.which == 13) {
        $("#sendButton").trigger('click');
        $("#userInput").focus();
      }
    });
    
    $("#clearButton").on('click',function(e){
        e.preventDefault();
        $("#history").text("");
        Shiny.onInputChange("userInput","");
        $("#userInput").focus();
    });
    
});