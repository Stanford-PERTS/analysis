treatment_effect_text <- list(
    effective = "PERTS is happy to report that " %+% n_completers %+%
        " students at " %+% org_name %+% " completed the Growth Mindset for College Students Program. " %+%
        " After the program, we found that these students reported a stronger growth mindset about learning. " %+% 
        " Figure 1 (below) shows the percentage of students who were thinking with a growth mindset before" %+%
        " and after the activity, as measured by scientifically-validated questions about growth mindset." %+% 
        " (e.g., “How much do you agree or disagree with the following statement: ‘You have a certain amount of intelligence, and you really can’t do much to change it’”) <sup>1</sup>",
    ineffective = "too bad.",
    insufficient_numbers =  "<p>Program results are not displayed due to low participation. " %+%
        "Only " %+% n_completers %+% " students from " %+% org_name %+%
        " have participated in the Growth Mindset for College Students program, " %+%
        " which falls short of the " %+%
        N_COMPLETERS_THRESHOLD %+% " participants required to assess program impact. " %+%
        "See the Participation Summary section " %+%
        "for details about participation at " %+% org_name %+% ".</p>"
    
)