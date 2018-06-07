var surveyHelper = {
    radioValue: function (name) {
        return $('input[name='+name+']:checked').val() || '';
    },

    oneChecked: function (name) {
        return $('input[name='+name+']:checked').length >= 1;
    },

    setWarning: function (id,value) {
        if (value) {
            $('#'+id+'_question').css('background','lightblue');
        } else {
            // $('#'+id+'_question').css('background','rgb(255,100,100)');
            $('#'+id+'_question').css('background','rgb(255,150,150)');
        }
    },

    showWarning: function (id,value) {
        if (value) {
            $('#'+id+'Warning').css('visibility','hidden');
        } else {
            $('#'+id+'Warning').css('visibility','visible');
        }
    },

    simpleSetWarning: function (id) {
        this.setWarning(id, this.oneChecked(id));
    },

    simpleShowWarning: function (id) {
        this.showWarning(id, this.oneChecked(id));
    },

    getInputValues: function (search) {
        var fields = $(search).serializeArray();
        var obj = {};
        // console.log(fields);
        $.each(fields, function (i, field) {
            if (field.name in obj) {
                obj[field.name] = obj[field.name] + ',' + field.value;
            } else {
                obj[field.name] = field.value;
            }
        });
        return obj;
    },

    textareaFilled: function (name) {
        return $('textarea[name='+name+']').val().length > 0;
    }

};

function trap_backspace (event) {
    var target = $(event.target);
    if (event.which === 8 && !target.is('textarea,input:text')) {
        event.preventDefault();
    }
}

function disable_backspace () {
    $(document).on("keydown", trap_backspace);
}

function fillInForm() {
    var id = getAssignmentId();
    var id_field = document.getElementById('myAssignmentId');
    var form = document.getElementById('experimentform');
    var sub = getSubmitTo();
    if (sub) {
        form.action = sub + '/mturk/externalSubmit';
    }
    id_field.value = id;
}

// For debugging.
function skipAround (ev) {
    var i;
    if (ev.which === 39) {
        ev.preventDefault();
        exp.nextScreen();
    } else if (ev.which === 37) {
        ev.preventDefault();
        // weirdness occurs if we hit the condition screen more than once.
        if (exp.current-1 >= 0 && exp.screens[exp.current-1].__proto__ !== GetConditionScreen.prototype) {
            exp.prevScreen();
        }
    // } else if (ev.which == 38) {
    //     ev.preventDefault();
    //     for (i=exp.current+1; i<exp.screens.length; i++) {
    //         if (exp.screens[i].__proto__ === KbdBlockScreen.prototype ||
    //             exp.screens[i].__proto__ === BlockScreen.prototype) {
    //             exp.jumpToScreen(i);
    //             return;
    //         }
    //     }
    //     exp.jumpToScreen(exp.screens.length-1);
    // } else if (ev.which == 40) {
    //     ev.preventDefault();
    //     for (i=exp.current-1; i>0; i--) {
    //         if (exp.screens[i].__proto__ === KbdBlockScreen.prototype ||
    //             exp.screens[i].__proto__ === BlockScreen.prototype) {
    //             exp.jumpToScreen(i);
    //             return;
    //         }
    //     }
    }
}

// base64 encoding
var _PADCHAR="=",_ALPHA="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
function base64Getbyte(s,i){var x=s.charCodeAt(i);if(x>255){throw"INVALID_CHARACTER_ERR: DOM Exception 5";}return x;}
function base64Encode(s){if(arguments.length!==1){throw"SyntaxError: exactly one argument required";}s=String(s);var i,b10,x=[],imax=s.length-s.length%3;if(s.length===0){return s;}for(i=0;i<imax;i+=3){b10=(base64Getbyte(s,i)<<16)|(base64Getbyte(s,i+1)<<8)|base64Getbyte(s,i+2);x.push(_ALPHA.charAt(b10>>18));x.push(_ALPHA.charAt((b10>>12)&63));x.push(_ALPHA.charAt((b10>>6)&63));x.push(_ALPHA.charAt(b10&63));}switch(s.length-imax){case 1:b10=base64Getbyte(s,i)<<16;x.push(_ALPHA.charAt(b10>>18)+_ALPHA.charAt((b10>>12)&63)+_PADCHAR+_PADCHAR);break;case 2:b10=(base64Getbyte(s,i)<<16)|(base64Getbyte(s,i+1)<<8);x.push(_ALPHA.charAt(b10>>18)+_ALPHA.charAt((b10>>12)&63)+_ALPHA.charAt((b10>>6)&63)+_PADCHAR);break;default:break;}return x.join("");}
