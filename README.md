## Dutch modality exclusivity norms for 336 properties and 411 concepts

This repository contains all experimental data, including every respondent's survey, the final data set in [Excel](https://github.com/pablobernabeu/Modality-exclusivity-norms-747-Dutch-English-replication/blob/master/norms.xlsx) or [CSV](https://github.com/pablobernabeu/Modality-exclusivity-norms-747-Dutch-English-replication/blob/master/all.csv) format, and the analysis code in R ([norms.R](https://github.com/pablobernabeu/Modality-exclusivity-norms-747-Dutch-English-replication/blob/master/norms.R)).

The norms, which are ratings of linguistic stimuli, served a twofold purpose: first, the creation of linguistic stimuli (Bernabeu, 2018; see also Speed & Majid, 2017), and second, a conceptual replication of Lynott and Connell's (2009, 2013) analyses. In the collection of the ratings, forty-two respondents completed surveys for the properties or the concepts separately. Each word was rated by eight participants on average (see [data set](https://osf.io/ge7pn/)), with a minimum of five (e.g., for *bevriezend*) and a maximum of ten ratings per word (e.g., for *donzig*). The [instructions to participants](https://osf.io/ungey/) were similar to those used by Lynott and Connell (2009, 2013), except that we elicited three modalities (auditory, haptic, visual) instead of five.

> <span style = "font-size: 14px;"> 'This is a stimulus validation for a future experiment. The task is to rate how much you experience everyday' [properties/concepts] 'using three different perceptual senses: feeling by touch, hearing and seeing. Please rate every word on each of the three senses, from 0 (not experienced at all with that sense) to 5 (experienced greatly with that sense). If you do not know the meaning of a word, leave it blank.' </span>

These norms were validated in an experiment showing that shifts across trials with different dominant modalities incurred semantic processing costs ([Bernabeu, Willems, & Louwerse, 2017](https://doi.org/10.31234/osf.io/a5pcz)). All data for that study are [available](https://osf.io/97unm/wiki/home/), including a [dashboard](https://pablobernabeu.shinyapps.io/ERP-waveform-visualization_CMS-experiment/) (in case of downtime of the dashboard site, please see [this alternative](https://mybinder.org/v2/gh/pablobernabeu/Modality-switch-effects-emerge-early-and-increase-throughout-conceptual-processing/master?urlpath=shiny/Shiny-app/)).

The properties and the concepts were analysed separately. Properties were more strongly perceptual than concepts. Distinct relationships also emerged among the modalities, with the visual and haptic modalities being closely related, and the auditory modality being relatively independent (cf. Lynott & Connell's data for English. This ties in with findings that, in conceptual processing, modalities can be collated based on language statistics (Louwerse & Connell, 2011).

The norms also served to investigate sound symbolism, which is the relation between the form of words and their meaning. The form of words rests on their sound more than on their visual or tactile properties (at least in spoken language). Therefore, auditory ratings should more reliably predict the lexical properties of words (length, frequency, distinctiveness) than haptic or visual ratings would. Lynott and Connell's (2013) findings were replicated, as auditory ratings were either the best predictor of lexical properties, or yielded an effect that was opposite in polarity to the effects of haptic and visual ratings.

The present analyses were reported in Bernabeu (2018). All data and analysis code for the norms are [available for re-use](https://osf.io/brkjw/wiki/home/) under a [CC-BY licence](https://creativecommons.org/licenses/by/4.0/), provided acknowledgment of the following publication:

> <span style = "font-size: 14.1px;"> Bernabeu, P. (2018). *Dutch modality exclusivity norms for 336 properties and 411 concepts*. PsyArXiv. https://psyarxiv.com/s2c5h </span>


**Abstract**

Part of the toolkit of language researchers is formed of stimuli that have been rated on various dimensions. The current study presents modality exclusivity norms for 336 properties and 411 concepts in Dutch. Forty-two respondents rated the auditory, haptic, and visual strength of these words. Mean scores were then computed, yielding acceptable reliability values. Measures of modality exclusivity and perceptual strength were also computed. Furthermore, the data includes psycholinguistic variables from other corpora, covering length (e.g., number of phonemes), frequency (e.g., contextual diversity), and distinctiveness (e.g., number of orthographic neighbours), along with concreteness and age of acquisition. To test these norms, Lynott and Connell’s (2009, 2013) analyses were replicated. First, unimodal, bimodal, and tri-modal words were found. Vision was the most prevalent modality. Vision and touch were relatively related, leaving a more independent auditory modality. Properties were more strongly perceptual than concepts. Last, sound symbolism was investigated using regression, which revealed that auditory strength predicted lexical properties of the words better than the other modalities did, or else with a different direction. All the data and analysis code, including a web application, are available from https://osf.io/brkjw/.

Further links:

#### [**Dashboard presenting the data and analyses**](https://pablobernabeu.shinyapps.io/dutch-modality-exclusivity-norms/) (in case of downtime, please see [alternative site](https://pablobernabeu.github.io/dashboards/Dutch-modality-exclusivity-norms/d.html))
 
![](https://i.imgur.com/gS1vpcM.gif)

#### [**Paper (Bernabeu, 2018)**](https://psyarxiv.com/s2c5h)

![The relation among auditory, haptic, and visual modalities in property and concept words, in English and Dutch.](https://raw.githubusercontent.com/pablobernabeu/Modality-exclusivity-norms-747-Dutch-English-replication/master/allfour_lowres.png)

- Online [RStudio environment with data and code](https://mybinder.org/v2/gh/pablobernabeu/Modality-exclusivity-norms-747-Dutch-English-replication/master?urlpath=rstudio).


### **Definitions** (as in Lynott & Connell, 2009, 2013)

- Dominant modality: Highest-rated modality;

- Modality exclusivity: Range of the three modality ratings divided by the sum;

- Perceptual strength: Highest rating across modalities.


### Corpus measures used

* Concreteness and age of acquisition: Brysbaert et al. (2014);

* Phonological and orthographic neighbours: Marian et al.'s (2012) DutchPOND;

* Word frequency and contextual diversity: Keuleers et al.'s (2010) SUBTLEX-NL;

* Lemma frequency: Baayen et al.'s (1993) CELEX.


### References

Baayen, R. H., Piepenbrock, R., & van Rijn, H. (1993). *The CELEX Lexical Database* [CD-ROM]. Philadelphia: Linguistic Data Consortium, University of Pennsylvania

Bernabeu, P. (2018). *Dutch modality exclusivity norms for 336 properties and 411 concepts*. PsyArXiv. https://psyarxiv.com/s2c5h

Bernabeu, P., Willems, R. M., & Louwerse, M. M. (2017). Modality switch effects emerge early and increase throughout conceptual processing: Evidence from ERPs. In G. Gunzelmann, A. Howes,  T. Tenbrink, & E. J. Davelaar (Eds.), *Proceedings of the 39th Annual Conference of the Cognitive Science Society* (pp. 1629-1634). Austin, TX: Cognitive Science Society. https://doi.org/10.31234/osf.io/a5pcz

Brysbaert, M., Warriner, A.B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. *Behavior Research Methods, 46*, 3, 904-911. <br>
[https://doi.org/10.3758/s13428-013-0403-5](https://doi.org/10.3758/s13428-013-0403-5)

Field, A. P., Miles, J., & Field, Z. (2012). *Discovering Statistics Using R*. London, UK: Sage

Keuleers, E., Brysbaert, M. & New, B. (2010). SUBTLEX-NL: A new frequency measure for Dutch words based on film subtitles. *Behavior Research Methods, 42*, 3, 643-650. [https://doi.org/10.3758/BRM.42.3.643](https://doi.org/10.3758/BRM.42.3.643)

K&ouml;hler, W. (1929). *Gestalt Psychology*. New York: Liveright

Louwerse, M., & Connell, L. (2011). A taste of words: Linguistic context and perceptual simulation predict the modality of words. *Cognitive Science, 35*, 2, 381-98. [https://doi.org/10.1111/j.1551-6709.2010.01157.x](https://doi.org/10.1111/j.1551-6709.2010.01157.x)

Lynott, D., & Connell, L. (2009). Modality exclusivity norms for 423 object concepts. *Behavior Research Methods, 41*, 2, 558-564. [https://doi.org/10.3758/BRM.41.2.558](https://doi.org/10.3758/BRM.41.2.558)

Lynott, D., & Connell, L. (2013). Modality exclusivity norms for 400 nouns: The relationship between perceptual experience and surface word form. *Behavior Research Methods, 45*, 2, 516-526. <br>
[https://doi.org/10.3758/s13428-012-0267-0](https://doi.org/10.3758/s13428-012-0267-0)

Marian, V., Bartolotti, J., Chabal, S., & Shook, A. (2012). CLEARPOND: Cross-Linguistic Easy-Access Resource for Phonological and Orthographic Neighborhood Densities. *PLoS ONE, 7*, 8: e43230. <br>
[https://doi.org/10.1371/journal.pone.0043230](https://doi.org/10.1371/journal.pone.0043230)

Sourav, S., Kekunnaya, R., Shareef, I., Banerjee, S., Bottari, D., & R&ouml;der, B. (2019). A protracted sensitive period regulates the development of cross-modal sound-shape associations in humans. *Psychological Science, 30*, 10, 1473-1482. [https://doi.org/10.1177/0956797619866625](https://doi.org/10.1177/0956797619866625)

Speed, L. J., & Majid, A. (2017). Dutch modality exclusivity norms: Simulating perceptual modality in space. *Behavior Research Methods, 49*, 6, 2204-2218. [https://doi.org/10.3758/s13428-017-0852-3](https://doi.org/10.3758/s13428-017-0852-3)


### Contact

Pablo Bernabeu. Email: p.bernabeu@lancaster.ac.uk.

[Webpage](http://www.research.lancs.ac.uk/portal/en/people/pablo-de-juan-bernabeu)

