function DemographicSurvey() {
    this.screen_id = 'DemographicSurvey';
    return this;
}

DemographicSurvey.prototype = {};

DemographicSurvey.prototype.baseSplitChoice = function (type, col, splits, num, id, question, options, warning) {
    warning = warning || 'Required';
    var i;
    var ret = '<div id="'+id+'_question" class="question '+col+'">';
    var pad = 'style="padding-left: 1em"';

    ret += '<div class="text">'+num.toString()+'. '+question+' </div>'; // <span id="'+id+'Warning" class="warning">'+warning+'</span>
    ret += '<table style="border-spacing:0">';
    for (i=0; i<options.length; i++) {
        var style;
        if (i%splits === 0) {
            ret += '<tr>';
            style = '';
        } else {
            style = pad;
        }
        ret += '<td '+style+'><label><input type="'+type+'" name="'+id+'" value="'+options[i][0].toString()+'"/> '+options[i][1].toString()+'</label></td>';
        if (i%splits === splits-1) { ret += '</tr>'; }
    }
    ret += '</table>';
    ret += '</div>';

    // console.log(ret);

    return ret;
};

DemographicSurvey.prototype.baseChoice = function (type, col, num, id, question, options, warning) {
    warning = warning || 'Required';
    var i;
    var ret = '<div id="'+id+'_question" class="question '+col+'">';

    ret += '<div class="text">'+num.toString()+'. '+question+' <span id="'+id+'Warning" class="warning">'+warning+'</span></div>';
    for (i=0; i<options.length; i++) {
        ret += '<label><input type="'+type+'" name="'+id+'" value="'+options[i][0].toString()+'"/> '+options[i][1].toString()+'</label><br>';
    }
    ret += '</div>';

    // console.log(ret);

    return ret;
};

DemographicSurvey.prototype.splitMultipleChoice = function (col, num, id, question, options, splits, warning) {
    return this.baseSplitChoice('radio', col, splits||2, num, id, question, options, warning);
};

DemographicSurvey.prototype.multipleChoice = function (col, num, id, question, options, warning) {
    return this.baseChoice('radio', col, num, id, question, options, warning);
};

DemographicSurvey.prototype.checkboxChoice = function (col, num, id, question, options, warning) {
    return this.baseChoice('checkbox', col, num, id, question, options, warning);
};

DemographicSurvey.prototype.textQuestion = function (col, num, id, question, warning) {
    warning = warning || 'Required';
    return ('<div class="question '+col+'">' +
            '<div class="text">'+num.toString()+'. '+question+'</div>'+
            '<input type="text" name="'+id+'"/> <span id="'+id+'Warning" class="warning">'+warning+'</span><br/>'+
            '</div>');
};

DemographicSurvey.prototype.addQuestions = function () {
    // Should we generate this form in js?
    $('#experiment_area').html('<div class="full-message-body" id="DemographicSurvey">'+
    '<div id="demographic_info" class="info">Please complete this demographic survey...</div>'+

    '<div class="left-column">'+

      // '<div class="lq2">'+

      '<table style="width: 100%; margin: 0 0 1px 0; padding: 0px; border-spacing: 0">'+

      '<tr><td id="gender_question" style="width: 50%; border-style: solid; border-color: rgb(230,240,255); border-width: 0 0 0 5px; background-color: lightblue;">'+
      '<div class="" style="display:block;  margin: 0px; padding: 5px; text-align: left">'+
        '<div class="text" >1. What is your gender? <!-- <span id="genderWarning" class="warning"> </span> --> </div>'+
        '<label><input type="radio" name="gender" value="male"/> Male</label><span style="display:inline-block; width: 1em"></span>'+
        '<label><input type="radio" name="gender" value="female"/> Female</label>'+
      '</div>'+

      '<td id="age_question" style="width: 50%; background-color: lightblue; border-style: solid; border-color: rgb(230,240,255); border-width: 0 5px 0 1px;">'+
       '<div class="" style="display:block; margin: 0; padding: 5px; text-align: left;">'+
        '<div class="text">2. What is your age?</div>'+
        '<input type="text" name="age"/> <!-- <span id="ageWarning" class="warning">Req</span> --> <br/>'+
      '</div>'+
      // '</div>'+
      '</table>'+
      '<div id="ethnicity_question" class="question lq">'+
        '<div class="text">3. What is your ethnicity? <span id="ethnicityWarning" class="warning">Required</span></div>'+
        '<label><input type="radio" name="ethnicity" value="white"/> White or Caucasian</label><br/>'+
        '<label><input type="radio" name="ethnicity" value="black"/> Black or African-American</label><br/>'+
        '<label><input type="radio" name="ethnicity" value="asian"/> Asian</label><br/>'+
        '<label><input type="radio" name="ethnicity" value="indian"/> American Indian or Alaska Native</label><br/>'+
        '<label><input type="radio" name="ethnicity" value="hawaiian"/> Native Hawaiian or Other Pacific Islander</label><br/>'+
        '<label><input type="radio" name="ethnicity" value="other"/> Other</abel> <input type="text" name="ethnicityOther"/> <span id="ethnicityOtherWarning" class="warning">Specify</span>'+
      '</div>'+

     this.splitMultipleChoice('lq', 4, 'played', 'Have you ever played video games?',
                         [['yes', 'Yes'],
                          ['no', 'No']]) +

     this.splitMultipleChoice('lq', 5, 'currentlyPlay', 'Do you currently play video games?',
                         [['yes', 'Yes'],
                          ['no', 'No']]) +

      this.splitMultipleChoice('lq', 6, 'howLong', 'For how long <span class="verb">have you played</span> video games?',
                               [['lt1month', 'Less than 1 month'],
                                ['1to4years', '1 to 4 years'],
                                ['1to6months', '1 to 6 months'],
                                ['4to8years', '4 to 8 years'],
                                ['6to12months', '6 to 12 months'],
                                ['gt8years', 'More than 8 years']],
                               2,
                               'Req') +



      

      // '<div class="question lq">'+
      //   '<div class="text">4. What is the highest level of education you\'ve completed?</div>'+
      //   '<select name="education">'+
      //     '<option value="selectone">Select one...</option>'+
      //     '<option value="lessThanHighschool">Less than Highschool</option>'+
      //     '<option value="highschool">Highschool/GED</option>'+
      //     '<option value="someCollege">Some College</option>'+
      //     '<option value="2year">2 Year College Degree (Associates)</option>'+
      //     '<option value="4year">4 Year College Degree (BA,BS)</option>'+
      //     '<option value="masters">Master\'s Degree</option>'+
      //     '<option value="phd">Doctoral Degree</option>'+
      //     '<option value="professional">Professional Degree (MD,JD)</option>'+
      //   '</select>'+
      //   '<span id="educationWarning" class="warning">Required</span>'+
      // '</div>'+

      // '<div class="question lq">'+
      //   '<div class="text">5. Are you a student at a college or university? <span id="studentWarning" class="warning">Req</span></div>'+
      //   '<label><input type="radio" name="student" value="no"/> No</label><br/>'+
      //   '<label><input type="radio" name="student" value="yes"/> Yes, my major is:</label> <input type="text" name="major"/> <span id="majorWarning" class="warning">Specify</span>'+
      // '</div>'+

      this.splitMultipleChoice('lq', 7, 'platform', 'Which gaming platform <span class="verb">do</span> you use the most?',
                               [['pc', 'PC'],
                                ['console', 'Console'],
                                ['handheld', 'Handheld Console'],
                                ['mobile', 'Mobile phone/tablet']], 2, 'Req')+

    '</div><div class="right-column">'+


      this.splitMultipleChoice('rq', 8, 'freq', 'How frequently <span class="verb">do</span> you play video games?',
                          [['everyday', 'Every day'],
                           ['monthly', 'Monthly'],
                           ['weekly', 'Weekly'],
                           ['irregularly', 'Irregularly']])+

      // this.checkboxChoice('rq', 7, 'relationship', 'Which statements describe your relationship with video games?  Check all that apply (if any)',
      //                     [['social', 'Video games provide a common social interest that I share with my friends.'],
      //                      ['challenge', 'I enjoy the mental and physical challenges video games provide.'],
      //                      ['competitive', 'I enjoy competitive games with human or computer opponents more than casual games.'],
      //                      ['skilled', 'I consider myself skilled (better than average) in at least one video game I play.']])+

      // this.textQuestion('rq', 8, 'hoursPlayed', 'How many hours a week do you spend playing video games?', 'Number Required') +

      this.splitMultipleChoice('rq', 9, 'hoursPlayed', 'How many hours a week <span class="verb">do</span> you play video games?',
                              [['lt1hr', '&lt; 1 Hour'],
                               ['1to5hr', '1-5 Hours'],
                              ['5to10hr', '5-10 Hours'],
                              ['10to20hr', '10-20 Hours'],
                              ['20to30hr', '20-30 Hours'],
                              ['gt30hr', '&gt; 30 Hours']], 3, '') +

      '<div id="relationship_question" class="question rq"><div class="text">10. Which statements describe your relationship with video games?  Check all that apply. <span id="relationshipWarning" class="warning">Required</span></div>'+
      '<table>'+
      '<tr><td style="vertical-align: top"><input id="social" type="checkbox" name="relationship" value="social"/><td><label for="social"> Video games provide a common social interest that I share with my friends.</label></tr>'+
      '<tr><td style="vertical-align: top"><input id="challenge" type="checkbox" name="relationship" value="challenge"/><td><label for="challenge"> I enjoy the mental and physical challenges video games provide.</label></tr>'+
      '<tr><td style="vertical-align: top"><input id="competitive" type="checkbox" name="relationship" value="competitive"/><td><label for="competitive"> I enjoy competitive games with human or computer opponents more than casual games.</label></tr>'+
      '<tr><td style="vertical-align: top"><input id="skilled" type="checkbox" name="relationship" value="skilled"/><td><label for="skilled"> I consider myself skilled (better than average) in at least one video game I play.</label></tr></table></div>'+


      // '<div class="question rq">'+
      //   '<div class="text">6. Have you taken the SAT or GRE? <span id="satWarning" class="warning">Required</span></div>'+
      //   '<label><input type="radio" name="takensat" value="no"/> No</label><br/>'+
      //   '<label><input type="radio" name="takensat" value="yes"/> Yes. Please fill in your scores as best as you recall:</label>'+
      //   '<table style="padding-left: 2em">'+
      //     '<tr><td style="text-align:right">Year:</td><td><input class="sat" type="text" name="satYear"/> <span id="satYearWarning" class="warning">Specify</span></td>'+
      //     '<tr><td style="text-align:right">Math:</td><td><input class="sat" type="text" name="satMath"/> <span id="satMathWarning" class="warning">Specify</span></td>'+
      //     '<tr><td style="text-align:right">Reading:</td><td><input class="sat" type="text" name="satReading"/> <span id="satReadingWarning" class="warning">Specify</span></td>'+
      //     '<tr><td style="text-align:right">Writing:</td><td><input class="sat" type="text" name="satWriting"/> <span id="satWritingWarning" class="warning">Specify</span></td>'+
      //   '</table>'+
      // '</div>'+

      // '<div class="question rq">'+
      //   '<div class="text">7. What grades do you usually get in math courses?</div>'+
      //   '<select name="grades">'+
      //     '<option value="selectone">Select one...</option>'+
      //     '<option value="a">mostly A\'s</option>'+
      //     '<option value="b">mostly B\'s</option>'+
      //     '<option value="c">mostly C\'s</option>'+
      //     '<option value="lower">mostly lower than C\'s</option>'+
      //   '</select>'+
      //   '<span id="gradesWarning" class="warning">Required</span>'+
      // '</div>'+

      // '<div class="question rq">'+
      //   '<div class="text">8. What is the most advanced math course you have taken?</div>'+
      //   '<input type="text" name="mathCourse"/>'+
      //   '<span id="mathCourseWarning" class="warning">Required</span>'+
      // '</div>'+

      '<div id="heard_question" class="question rq">'+
        '<div class="text">11. Did you hear about this study from anyone? <span id="heardWarning" class="warning">Req</span></div>'+
        '<label><input type="radio" name="heard" value="no"/> No</label><br/>'+
        '<label><input type="radio" name="heard" value="yes"/> Yes. Please describe what you heard.</label> <span id="heardDescriptionWarning" class="warning">Specify</span>'+
        '<br/>'+
        '<textarea name="heardDescription" rows="4"></textarea>'+
      '</div>'+

    '</div>'+

    '<div class="demographic-submit-area">'+
      '<input type="button" value="Continue" onClick="submitDemographicSurvey();">'+
    '</div>'+
  '</div>'+
'</div>');
    $("#played_question input[name=played]").on('click', $.proxy(this.clickPlayed, this));
    $("#currentlyPlay_question input[name=currentlyPlay]").on('click', $.proxy(this.clickCurrentlyPlay, this));
};

DemographicSurvey.prototype.clickPlayed = function () {
    var v = $("#played_question input[name=played]:checked").val();

    if (v == 'yes') {
        this.showGameQuestions();
    } else {
        this.hideGameQuestions();
    }
};

DemographicSurvey.prototype.clickCurrentlyPlay = function () {
    var v = $("#currentlyPlay_question input[name=currentlyPlay]:checked").val();
    var verb = (v == 'yes') ? 'do':'did';
    var phrase = (v == 'yes') ? 'have you played':'did you play';

    // console.log('clicked');

    $('#DemographicSurvey span.verb').html(verb);
    $('#howLong_question span.verb').html(phrase);
};

DemographicSurvey.prototype.hideGameQuestions = function () {
    // $("#DemographicSurvey div.left-column div.question").slice(3,6).css('visibility', 'hidden');
    // $("#DemographicSurvey div.right-column div.question").slice(0,3).css('visibility', 'hidden');

    // $("#DemographicSurvey div.left-column div.question").slice(3,6).fadeOut("slow");
    // $("#DemographicSurvey div.right-column div.question").slice(0,3).fadeOut("slow");

    $("#DemographicSurvey div.left-column div.question").slice(2,6).slideUp();
    $("#DemographicSurvey div.right-column div.question").slice(0,3).slideUp();
    // console.log('hide');
};

DemographicSurvey.prototype.showGameQuestions = function () {
    $("#DemographicSurvey div.left-column div.question").slice(2,6).slideDown();
    $("#DemographicSurvey div.right-column div.question").slice(0,3).slideDown();
    // console.log('show');
};

DemographicSurvey.prototype.init = function () {
    this.addQuestions();
    exp.lg('start');
};

// DemographicSurvey.prototype.oneChecked = function (name) {
//     return $('input[name='+name+']:checked').length >= 1;
// };

// DemographicSurvey.prototype.radioValue = function (name) {
//     return $('input[name='+name+']:checked').val() || '';
// };

// DemographicSurvey.prototype.setWarning = function (id,value) {
//     if (value) {
//         $('#'+id+'_question').css('background','lightblue');
//     } else {
//         // $('#'+id+'_question').css('background','rgb(255,100,100)');
//         $('#'+id+'_question').css('background','rgb(255,150,150)');
//     }
// };

// DemographicSurvey.prototype.showWarning = function (id,value) {
//     if (value) {
//         $('#'+id+'Warning').css('visibility','hidden');
//     } else {
//         $('#'+id+'Warning').css('visibility','visible');
//     }
// };

// DemographicSurvey.prototype.simpleSetWarning = function (id) {
//     this.setWarning(id, this.oneChecked(id));
// };

// DemographicSurvey.prototype.simpleShowWarning = function (id) {
//     this.showWarning(id, this.oneChecked(id));
// };

DemographicSurvey.prototype.setErrorMessage = function (msg) {
    $('#demographic_info').addClass('error').html(msg);
};

DemographicSurvey.prototype.check = function () {
    survey = $('#DemographicSurvey');
    var gender = surveyHelper.oneChecked('gender');
    var age_val = $('#DemographicSurvey input[name=age]').val();
    var age = age_val.length > 0 && Number(age_val) > 0;

    var ethnicityOther = surveyHelper.radioValue('ethnicity') != 'other' || $('input[name=ethnicityOther]').val().length > 0;
    var ethnicity = surveyHelper.oneChecked('ethnicity') && ethnicityOther;

    var played = surveyHelper.oneChecked('played');

    var extra_is_good = true;

    if (!played || surveyHelper.radioValue('played') == 'yes') {
        var curPlay = surveyHelper.oneChecked('currentlyPlay');
        var howLong = surveyHelper.oneChecked('howLong');
        var platform = surveyHelper.oneChecked('platform');
        var freq = surveyHelper.oneChecked('freq');
        var hoursPlayed = surveyHelper.oneChecked('hoursPlayed');
        // var relationship = surveyHelper.oneChecked('relationship');

        surveyHelper.simpleSetWarning('currentlyPlay');
        surveyHelper.simpleSetWarning('howLong');
        surveyHelper.simpleSetWarning('platform');
        surveyHelper.simpleSetWarning('freq');
        surveyHelper.simpleSetWarning('hoursPlayed');
        // surveyHelper.simpleSetWarning('relationship');
        extra_is_good = curPlay && howLong && platform && freq && hoursPlayed;
    }

    var heardDescription = surveyHelper.radioValue('heard') != 'yes' || $('textarea[name=heardDescription]').val().length > 0;
    var heard = surveyHelper.oneChecked('heard') && heardDescription;

    surveyHelper.simpleSetWarning ('gender');
    surveyHelper.setWarning ('age', age);
    surveyHelper.simpleSetWarning ('ethnicity');
    surveyHelper.showWarning ('ethnicityOther', ethnicityOther);
    surveyHelper.simpleSetWarning('played');
    surveyHelper.simpleSetWarning ('heard');
    surveyHelper.showWarning ('heardDescription', heardDescription);

    return gender && age && ethnicity && played && extra_is_good && heard;
};

DemographicSurvey.prototype.submit = function () {
    if (this.check()) {
        exp.lg('end', {'survey': surveyHelper.getInputValues('#DemographicSurvey :input')});
        exp.nextScreen();
    } else {
        this.setErrorMessage("Please answer the highlighted questions then click Continue.");
    }
};

DemographicSurvey.prototype.cleanup = function () {
};

function submitDemographicSurvey() {
    exp.currentScreen().submit();
}
