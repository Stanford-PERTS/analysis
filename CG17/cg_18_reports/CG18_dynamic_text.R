change_ideas <- list(
    relevance_of_coursework = "If you’d like to support your students in understanding the relevance of their coursework, here are a few resources you might consider the following:" %+%
        "<ul>" %+%
        "<li>The PERTS mindset kit (<a href='www.mindsetkit.org' target='_blank'>www.mindsetkit.org</a>)
            offers a free <a href='https://www.mindsetkit.org/search?q=' target='_blank'>practice library</a>
            for finding and sharing teaching practices that support students’ motivation,
            including <a href='https://www.mindsetkit.org/search?q=&tags=Purpose%20%26%20Relevance'>relevance.</a>
            Although the Mindset Kit was originally developed for K-12 teachers, we have heard from
            many college instructors across the country who find its resources useful in their own
            classes.</li>" %+%
        "<li>Next year, PERTS will have tools available to track progress in students' understanding of the relevance of their coursework. If you would like to be put on a contact list for when these tools are available, please email contact@perts.net!</li>" %+%
        "</ul>",
    belonging = "If you’d like to support your students in feeling a sense of belonging, please consider the following:" %+%
        "<ul>" %+%
        "<li>Together, PERTS and the College Transition Collaborative (<a href='http://collegetransitioncollaborative.org/'>http://collegetransitioncollaborative.org/</a>)
            is now offering a free, evidence-based online program to support new students in developing
            a secure sense of belonging to a limited number of colleges. For more information, visit
            <a href='https://www.perts.net/orientation/cb17'>https://www.perts.net/orientation/cb17</a>." %+%
        "<li>The PERTS mindset kit (<a href='https://www.mindsetkit.org/' target='_blank'>www.mindsetkit.org</a>)
                offers a section devoted to belonging <a href='https://www.mindsetkit.org/belonging' target='_blank'>www.mindsetkit.org/belonging</a>
                Although the Mindset Kit was originally developed for K-12 teachers, we have heard from
                many college instructors across the country who find its resources useful in their own
                classes.</li>" %+%
        "<li>The Mindset Kit Practice Library also has <a href='https://www.mindsetkit.org/search?q=&tags=Belonging' target='_blank'>free lesson ideas for promoting belonging</a>.</li>" %+%
        "<li>Next year, PERTS will have tools available to track progress in students' feelings of belonging. If you would like to be put on a contact list for when these tools are available, please email contact@perts.net!</li>" %+%
        "</ul>",
    instructor_caring = "If you'd like to support your students in understanding their instructors care, please consider the following:" %+%
        "<ul>" %+%
        "<li>The PERTS mindset kit (www.mindsetkit.org) offers a section devoted to belonging <a href='https://www.mindsetkit.org/belonging' target='_blank'>www.mindsetkit.org/belonging</a>.
            This section may also be relevant to helping students feel cared for by instructors.
            Although the Mindset Kit was originally developed for K-12 teachers, we have heard from
            many college instructors across the country who find its resources useful in their own
            classes.</li>" %+%
        "<li>Next year, PERTS will have tools available to track progress in how much students feel that adults at school care for them. If you would like to be put on a contact list for when these tools are available, please email contact@perts.net!</li>" %+%
        "</ul>",
    community_of_learners = "If you'd like to support your students in feeling like a part of a community of learners, please consider the following:" %+%
        "<ul>" %+%
        "<li>The Carnegie Foundation has recommendations for creating positive learning interactions between students, including</li>" %+%
            "<ul>" %+%
            "<li><b>Student Noticing Routine.</b> In this routine, groups of students
                take responsibility for contacting students in their group who are missing in
                order to encourage them to attend future classes and give them any materials or
                information that they missed from class. </li>" %+%
            "<li>Next year, PERTS will have tools available to track progress in how much
                students feel that adults at school care for them. If you would like to
                be put on a contact list for when these tools are available,
                please email contact@perts.net!</li>" %+%
            "</ul>" %+%
        "</ul>"
)

lc_desc <- list(
    "belonging" = list(
        "description" = "When students feel socially connected, supported, and respected,
        they are less distracted by insecurities and more likely to engage in learning effectively.
        See [mindsetscholarsnetwork.org/learning-mindsets/belonging/](http://mindsetscholarsnetwork.org/learning-mindsets/belonging).
        ",
        "introduction" = "
        <p>
        Students who are confident they belong and are valued by their teachers and
        peers are able to engage more fully in learning. They have fewer behavior
        problems, are more open to critical feedback, take greater advantage of
        learning opportunities, build important relationships, and generally have
        more positive attitudes about their classwork and teachers. In turn, they
        are more likely to persevere in the face of difficulty and do better in school.
        <br><br>
        When students are uncertain about whether they belong, they are vigilant
        for cues in the environment that signal whether or not they belong, fit in,
        or are welcome there. They may also be concerned about confirming a negative
        stereotype about their group. This hyper-vigilance and extra stress uses up
        cognitive resources that are essential for learning, diminishing their
        performance and discouraging them from building valuable relationships with
        peers and instructors.
        </p>
        <p><b>Recommendations</b><br>
        To learn specific strategies for promoting belonging, visit
        the [PERTS Mindset Kit Belonging Course](http://www.mindsetkit.org/belonging).
        </p>

        "
    ),
    "relevance_of_coursework" = list(
        "description" = "When students understand the value and relevance of their
            schoolwork, they are better able to learn deeply and resist distractions.
            See
            [mindsetscholarsnetwork.org/learning-mindsets/purpose-relevance/](http://mindsetscholarsnetwork.org/learning-mindsets/purpose-relevance/).",
        "introduction" = ""
    ),
    "instructor_caring" = list(
        "description" = "Students engage more deeply in their work when they
            feel like their instructors know and care about them.",
        "introduction" = ""
    ),
    "community_of_learners" = list(
        "description" = "When college students form relationships with other students
            that support academic achievement, this enhances students' sense of belonging
            and helps them stay engaged through difficult coursework. Developing
            a community of learners goes beyond friendship: it is critical
            for students to form relationships that <i>support academic achievement</i>."
    )
)

branch_text_shared_chunk <- paste0(
  "Before group M = " ,specific_school_stats$mean_gms_pre, ", After group M = " ,specific_school_stats$mean_gms_post, " points on a 6-point scale, &beta; = " ,specific_school_stats$b_effect, ", SE = " ,specific_school_stats$se_effect, ", t(" ,specific_school_stats$df_effect, ") = " ,specific_school_stats$t_effect, ", p " ,specific_school_stats$p_effect_txt, ""
)
branch_text_sign_different_from_overall_chunk <- paste0(
  "&beta; = " ,specific_school_stats$effect_different_from_overall_b, ", SE = " , specific_school_stats$effect_different_from_overall_se, ", t(" , specific_school_stats$effect_different_from_overall_df, ") = " , specific_school_stats$effect_different_from_overall_t, ", p " , specific_school_stats$effect_different_from_overall_p_txt, ""
)

branch_text <- list(
  '1' = paste0(
          "As in the overall sample, the <b>positive impact of Growth Mindset for College Students was statistically significant</b>. ",
          "(", branch_text_shared_chunk, ").",
          "<sup><a href='#fn3' id='ref3'>3</a></sup>"
        ),
  '2' = paste0(
          "The mean effects of Growth Mindset for College Students at ", school_name, "  <b>were consistent with — and statistically indistinguishable from — the overall treatment effect</b>. ",
          "(", branch_text_shared_chunk, ").",
          "<sup><a href='#fn3' id='ref3'>3</a></sup>"
        ),
  '3' = paste0(
          "As in the overall sample, the <b>positive impact of Growth Mindset for College Students was statistically significant</b>. ",
          " (", branch_text_shared_chunk, ").",
          " Furthermore, it was significantly <b>stronger than in the overall sample</b>, ",
          "(", branch_text_sign_different_from_overall_chunk, ").",
          "<sup><a href='#fn3' id='ref3'>3</a></sup>",
          "<br><br>We are interested in learning more about colleges like yours. It is possible that something about the way in which you implemented the program could account for the stronger effects at your school. For example, your faculty may have done an especially good job of framing the program for students. Or perhaps another initiative at your school worked synergistically with Growth Mindset for College Students.<br><br><b>If you or someone at your college are willing to be interviewed by a member of PERTS</b>, so that we can understand what factors may have led to especially powerful effects at your college, <b>please email support@perts.net</b> and let us know. Our goal is to maximize the benefit to students across the country, and it is possible that other colleges could learn from you how to best use our program to serve their students."
        ),
  '4' = paste0(
          "At ",school_name , ", the program’s results were less strong than they were in the overall sample. (Before group M = ", specific_school_stats$mean_gms_pre, ", After group M = ", specific_school_stats$mean_gms_post, " on a 6-point scale, difference from overall effect ", branch_text_sign_different_from_overall_chunk, ".<sup><a href='#fn3' id='ref3'>3</a></sup>) It is unclear whether this difference reflects a true difference in the program impact at your school or whether the statistical difference was a random type 1 error. In light of that uncertainty, we recommend three courses of action.<br><br>First, try the program again to ascertain whether these unusual results were a “fluke” or whether they represent a reliable variation in effect. Given the extensive work and testing that has gone into Growth Mindset for College Students, it is likely that these unusual results can be attributed to random variation. However, it is certainly possible that the program has a different impact at your school because of differences in the school context or in the way the program was administered (read on below). If you find that the program consistently does not work at your school, we recommend you stop using it! However, we think that’s unlikely, and we recommend trying again after reviewing the program administration procedures.<br><br>Second, we recommend reviewing the procedures that are used to administer the program. Differences in program administration can lead to differences in how students respond to the program materials. For example, if students are told that the program is supposed to help them develop a growth mindset, or if they are aware that only remedial students are being exposed to the program, this can provoke a defensive reaction in many students (“why do they think I need a growth mindset? Is it because I’m not cut out for college?”). This can undermine the effectiveness of the program — or even lead it to backfire. We recommend you review your administration procedures and compare them to the best practices outlined in the program administration materials.<br><br>Finally, you might consider interviewing some students about their experience with the program and the way it was administered. This could point you to changes you could make in the way the program is introduced and overseen at your school, or it could point to changes we should make at PERTS to improve the content of the program. We’re always working to improve our programs so that they work more effectively for more students in more contexts. So, if you discover new insights about the way the program was received by your students, we encourage you to share those insights with us by contacting support@perts.net."
        ),
	'5' = paste0(
          "Nevertheless, the mean effects of Growth Mindset for College Students at ", school_name, "  <b>were consistent with — and statistically indistinguishable from — the overall positive treatment effect</b>. ",
          "(", branch_text_shared_chunk, ").",
          "<sup><a href='#fn3' id='ref3'>3</a></sup>"
        )
)


cg18_text = list(
  "chunk_1" = paste0(
    "<h2>Introduction to the Report</h2>" ,
    "Thank you for participating in Growth Mindset for College Students! This report is provided by the <a href='https://www.perts.net'>Project for Education Research that Scales (PERTS)</a> to describe the program’s impact on students at " , school_name , ", to let you know how many students at " ,
    school_name , " participated in the program, and to offer insight into other actionable psychological barriers that may be affecting in your student body.<h2>Impact of Growth Mindset for College Students</h2>PERTS is happy to report that " , overall_stats$n_overall , " across the country and " , specific_school_stats$n, " students at " , school_name , " completed Growth Mindset for College Students. Overall, <b>the program positively impacted students’ levels of growth mindset in this sample.</b><sup><a href='#fn1' id='ref1'>1</a></sup> We assessed this impact by randomly selecting students to answer scientifically-validated growth mindset questions (e.g., &#8220; How much do you agree or disagree with the following statement: ‘You have a certain amount of intelligence, and you really can’t do much to change it’&#8221;) either <i>before</i> the growth mindset activity (the &#8220;Before Group&#8221;) or <i> after</i>  (the &#8220;After Group&#8221;) <sup><a href='#fn2' id='ref2'>2</a></sup>. " ,
    " Before the intervention, ",overall_stats$overall_perc_good_pre,
    "% of students reported thinking with a growth mindset while ",overall_stats$overall_perc_good_post,
    "% of students reported thinking in a growth mindset after the intervention.",
    " The average difference was highly statistically significant, &beta; =  " , overall_stats$lmer_stats$est , ", t(" , overall_stats$lmer_stats$df , ") = " , overall_stats$lmer_stats$t , ",  p " , overall_stats$lmer_stats$p ,".<sup><a href='#fn3' id='ref3'>3</a></sup>"
  ),
  "chunk_2" = paste0(
    "<h2>Impact at " ,school_name, "</h2> Of the " ,specific_school_stats$n, " students from ", school_name, " who participated in Growth Mindset for College Students, " ,specific_school_stats$post_perc_good, "% in the After Group (n = " ,specific_school_stats$n_post, ") reported thinking with a growth mindset, compared with " ,specific_school_stats$pre_perc_good, "% of students in the Before Group (n = " ,specific_school_stats$n_pre, ") — a change of ", specific_school_stats$pct_good_change,"% points.<sup><a href='#fn4' id='ref4'>4</a></sup> "
  ),
  "chunk_3" = branch_text[[specific_school_stats$results_branch]],
  "footnote_1" = paste0(
    "<sup id='fn1'>1.  We do not make claims about the effectiveness of the Growth Mindset for College Students program for promoting growth mindset <i>in general</i>. It is well known that contextual factors are critically important for determining the effectiveness of interventions in particular samples, sites, etc. (Bryk, Gomez, Grunow & LeMahieu, 2015), and we do not know how far the overall results would generalize to new institutions. The overall sample results are provided solely for the purpose of helping ",
    school_name,
    " contextualize their results in terms of the broader ",
    YEAR,
    " cohort of participants.    <a href='#ref1' title='Jump back to footnote 1 in the text.'>&#8617;</a></sup>"
  ),
  "footnote_2" = paste0(
    "<sup id='fn2'>2. Because there was differential attrition in the sample of students who answered the growth mindset questions before vs. after the growth mindset activity (",overall_stats$attrition_stats$raw_pre_perc, "% of students answered them before vs. ", overall_stats$attrition_stats$raw_post_perc, "% answered them after, &chi;<sup>2</sup>(1) = ",overall_stats$attrition_stats$raw_chi_sq_est, ", p ",overall_stats$attrition_stats$raw_chi_sq_p.value, "), we filtered all analyses to only those students who completed the entire program start to finish. This sub-sample showed no differential attrition (",overall_stats$attrition_stats$compl_pre_perc, "% of students in the control group vs. ",overall_stats$attrition_stats$compl_post_perc, "% in the treatment group finished the entire program, &chi;<sup>2</sup>(1) = ",overall_stats$attrition_stats$compl_chi_sq_est, ", p ",overall_stats$attrition_stats$compl_chi_sq_p.value, ").
<a href='#ref2' title='Jump back to footnote 2 in the text.'>&#8617;</a></sup>"
  ),

  "footnote_3" = paste0(
    "<sup id='fn3'>3. All statistical models fit a fixed effect indicating randomly-assigned membership in the “Before” or “After” group and no other fixed effects. This variable can be interpreted as measuring the effect of the program on students’ levels of growth mindset. The dependent variable was continuous growth mindset scale scores (ranging from 1 - 6 points). The model used for the ", school_name, " only sample was a simple linear regression with no random effects or covariates. The full-sample analysis fitted a mixed-effects model with a fixed effect of group (before vs. after) and random intercepts for each college, to adjust for inter-college variability in baseline growth mindset.<a href='#ref3' title='Jump back to footnote 3 in the text.'>&#8617;</a></sup>"
  ),
  "footnote_4" = paste0(
    "<sup id='fn4'>4. We strongly discourage comparing the base rates of growth mindset at your school with base rates in the overall sample. This comparison is not meaningful because the sample is heterogeneous — over 100 colleges enrolled students, each according to their own participation criteria. For example, some colleges may have enrolled students in remedial programs, whereas others may have enrolled students in honors programs. It is not possible to know how students at your college ought to compare to this sample <sup><a href='#fn1' id='ref1'>1</a></sup>. Instead, we suggest it is meaningful to learn how the program led to changes in mindset at your college. Differences in effects at ", school_name, " compared with the overall sample were examined using model identical to the overall sample which included terms for membership in ", school_name, " vs. the rest of the sample, as well as the interaction between group (Before vs. After) and membership in ", school_name, ". A statistically significant interaction term is interpreted to show a distinct program impact at ", school_name, ".<a href='#ref4' title='Jump back to footnote 4 in the text.'>&#8617;</a></sup>"
  )
)

treatment_effect_text <- list(
  insufficient_numbers =  "<p>Program results are not displayed due to low participation. " %+%
    "Only " %+% specific_school_stats$n %+% " students from " %+% org_name %+%
    " have participated in the Growth Mindset for College Students program, " %+%
    " which falls short of the " %+%
    N_COMPLETERS_THRESHOLD %+% " participants required to assess program impact. " %+%
    "See the Participation Summary section " %+%
    "for details about participation at " %+% org_name %+% ".</p>"
)
# if there is only 1 student, replace the plural form
treatment_effect_text$insufficient_numbers <- treatment_effect_text$insufficient_numbers %>% gsub(" 1 students", " 1 student",.)


# no quotes message
quotes_warning_explanation <- paste0("<br>Due to confidentiality concerns, quotes from students may only be displayed if at least ", N_COMPLETERS_THRESHOLD, " students participate. At ", school_name, " only ", specific_school_stats$n, " participated. Therefore, individual student quotes are not displayed.<br>")
