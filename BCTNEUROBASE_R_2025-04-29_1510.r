#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('BCTNEUROBASE_DATA_2025-04-29_1510.csv')
#Setting Labels

label(data$record_id)="Record ID"
label(data$criteres_inclusion___1)="Criteres dinclusion :  (choice=Admission en Réa / USC / SSPI < = 48h entrée hôpital)"
label(data$criteres_inclusion___2)="Criteres dinclusion :  (choice=Agression neurologique aiguë)"
label(data$criteres_inclusion___3)="Criteres dinclusion :  (choice=Âge > 17 ans)"
label(data$criteres_inclusion___4)="Criteres dinclusion :  (choice=Non-opposition)"
label(data$centre_inclusion)="Centre dinclusion"
label(data$traumabase)="Patient inclus dans la Traumabase ?"
label(data$inclusion_tb_numero)="inclusion_tb_numero"
label(data$invnam)="Investigateur recruteur"
label(data$rfstdtc)="Date dinclusion"
label(data$subjlnam)="Initiale nom"
label(data$subjfnam)="Initiale prénom"
label(data$sex)="Sexe"
label(data$femme_peripartum)="Grossesse en cours ou post-partum immédiat ?"
label(data$poids)="Poids"
label(data$taille)="Taille (en cm)"
label(data$more_than_2_years_old)="Le patient a til plus de 2 ans?"
label(data$age)="Age approximatif à linclusion"
label(data$ieyn)="Le patient répond-il bien à tous les critères déligibilité?"
label(data$donnes_dmographiques_aphp_complete)="Complete?"
label(data$hta)="Hypertension arterielle"
label(data$hta_traitement)="HTA traitée"
label(data$diabete)="Diabete"
label(data$tabac)="Tabagisme"
label(data$intox_chronique___1)="Intoxication chronique (choice=Aucune)"
label(data$intox_chronique___2)="Intoxication chronique (choice=Alcool)"
label(data$intox_chronique___3)="Intoxication chronique (choice=Cannabis)"
label(data$intox_chronique___4)="Intoxication chronique (choice=Autre)"
label(data$intox_chronique___5)="Intoxication chronique (choice=Non disponible)"
label(data$intox_chronique_autre)="Autre - Precisez"
label(data$intox_aigue___1)="Intoxication aigue (choice=Aucune)"
label(data$intox_aigue___2)="Intoxication aigue (choice=Alcool)"
label(data$intox_aigue___3)="Intoxication aigue (choice=Cannabis)"
label(data$intox_aigue___4)="Intoxication aigue (choice=Autre)"
label(data$intox_aigue___5)="Intoxication aigue (choice=Non disponible)"
label(data$intox_aigue_autre)="Autre - Precisez"
label(data$aap)="Antiagregants plaquettaires"
label(data$aap_type___1)="Type dantiagregant plaquettaire (choice=Aspirine)"
label(data$aap_type___2)="Type dantiagregant plaquettaire (choice=Clopidogrel (PLAVIX))"
label(data$aap_type___3)="Type dantiagregant plaquettaire (choice=Ticlodipine (TICLID))"
label(data$aap_type___4)="Type dantiagregant plaquettaire (choice=Prasugrel (EFIENT))"
label(data$aap_type___5)="Type dantiagregant plaquettaire (choice=Ticagrelor (BRILIQUE))"
label(data$aap_type___6)="Type dantiagregant plaquettaire (choice=Anti-Gp-IIb-IIIa (TIROFIBAN))"
label(data$aap_type___7)="Type dantiagregant plaquettaire (choice=Cangrelor (KENGREXAL))"
label(data$lateralite)="Lateralite"
label(data$anticoagulant)="Anticoagulants"
label(data$anticoagulant_type___1)="Type danticoagulant (choice=Anticoagulant oral direct)"
label(data$anticoagulant_type___2)="Type danticoagulant (choice=Antivitamine K)"
label(data$anticoagulant_type___3)="Type danticoagulant (choice=HBPM)"
label(data$anticoagulant_type___4)="Type danticoagulant (choice=HNF)"
label(data$francophone)="Patient.e francophone ?"
label(data$isolement)="Isolement social ?"
label(data$activite)="Activite"
label(data$rankin_score)="Score modifié de Rankin  0 - Pas de symptomes  1 - Symptomes sans handicap  2 - Handicap leger : autonome  3 - Handicap modere : marche seul  4 - Handicap severe : assistance a la marche  5 - Handicap majeur : incontinent, confine au lit"
label(data$antecedents_complete)="Complete?"
label(data$date_symptomes)="Début des symptomes (jour et heure) Si heure complètement inconnue, renseigner 11:11"
label(data$date_pec)="Date de la première prise en charge (premières constantes) "
label(data$pas_initiale)="Pression arterielle systolique (mmHg)"
label(data$pad_initiale)="Pression arterielle diastolique (mmHg)"
label(data$pam_initiale_calculee)="Pression artérielle calculée (en mmHg)"
label(data$fc_initiale)="Frequence cardiaque (/min)"
label(data$spo2_initiale)="Saturation en oxygene (%)"
label(data$temp_initiale)="Temperature (°C)"
label(data$pas_inf_90)="Episode de PAS < 90 mmHg ?"
label(data$spo2_inf_90)="Episode de SpO2 < 90 % ?"
label(data$gcs_detaille)="Score de Glasgow détaillé (YVM)"
label(data$gcs_total_prerea_admission)="Score de Glasgow total"
label(data$gcs_yeux_initial)="Score de Glasgow - Yeux"
label(data$gcs_verbal_initial)="Score de Glasgow - Verbal"
label(data$gcs_moteur_initial)="Score de Glasgow - Moteur"
label(data$gcs_total_detail_admission)="Score de Glasgow total"
label(data$convulsions_initial)="Convulsions"
label(data$pupille_gauche_admission_v2)="Pupille gauche"
label(data$pupille_droite_admission_v2)="Pupille droite"
label(data$dtc_initial_fait)="Doppler transcraniens faits ?"
label(data$bf_dtc_initial)="Backflow au Doppler ?"
label(data$bf_dtc_initial_cote___1)="Localisation du backflow ? (choice=Gauche)"
label(data$bf_dtc_initial_cote___2)="Localisation du backflow ? (choice=Droit)"
label(data$dtc_g_initial)="Doppler transcraniens gauche faisables ?"
label(data$dtc_g_vs_initial)="Vitesse systolique (cm/s)"
label(data$dtc_g_vd_initial)="Vitesse diastolique (cm/s)"
label(data$dtc_g_ip_initial)="Index de pulsatilite"
label(data$dtc_d_initial)="Doppler transcraniens droit faisables ?"
label(data$dtc_d_vs_initial)="Vitesse systolique (cm/s)"
label(data$dtc_d_vd_initial)="Vitesse diastolique (cm/s)"
label(data$dtc_d_ip_initial)="Index de pulsatilite"
label(data$imagerie_prerea)="Imagerie avant le transfert en reanimation"
label(data$imagerie_prerea_type___1)="Type dimagerie (choice=TDM)"
label(data$imagerie_prerea_type___2)="Type dimagerie (choice=IRM)"
label(data$imagerie_prerea_injection)="Imagerie injectee"
label(data$malformation_vasculaire)="Malformation vasculaire"
label(data$malformation_vasculaire_type___1)="Type de malformation vasculaire (choice=Anevrysme arteriel)"
label(data$malformation_vasculaire_type___2)="Type de malformation vasculaire (choice=Malformation arterio-veineuse)"
label(data$malformation_vasculaire_type___3)="Type de malformation vasculaire (choice=Fistule durale)"
label(data$osmo_prerea)="Osmotherapie"
label(data$osmo_prerea_indication___1)="Indication de losmotherapie (choice=Anomalie pupillaire)"
label(data$osmo_prerea_indication___2)="Indication de losmotherapie (choice=Doppler pathologiques)"
label(data$osmo_prerea_indication___3)="Indication de losmotherapie (choice=Degradation neurologique rapide)"
label(data$osmo_prerea_type___1)="Soluté dosmotherapie (choice=Mannitol)"
label(data$osmo_prerea_type___2)="Soluté dosmotherapie (choice=Serum sale hypertonique)"
label(data$osmo_prerea_efficace)="Regression de lanomalie pupillaire apres losmotherapie ?"
label(data$intubation_prerea)="Intubation"
label(data$etco2_dispo)="EtCO2 disponible ?"
label(data$etco2_prerea)="Premier EtCO2 apres intubation (mmHg)"
label(data$norad_prerea)="Noradrenaline"
label(data$norad_prerea_indication)="Indication de la Noradrenaline"
label(data$acr_prehosp)="Arret cardiaque prehospitalier"
label(data$prise_en_charge_avant_la_reanimation_complete)="Complete?"
label(data$date_admission)="Admission en reanimation / USC / SSPI (date et heure)"
label(data$mode_admission)="Mode dadmission"
label(data$admission_ggnc_pdses)="Type dadmission"
label(data$pas_admission)="Pression arterielle systolique (mmHg)"
label(data$pad_admission)="Pression arterielle diastolique (mmHg)"
label(data$pam_admission)="Pression arterielle moyenne (mmHg)"
label(data$pam_admission_calculee)="Pression arterielle moyenne calculée (mmHg)"
label(data$fc_admission)="Frequence cardiaque (/min)"
label(data$spo2_admission)="Saturation en oxygene (%)"
label(data$temp_admission)="Temperature (en °C)"
label(data$sedation_admission)="Malade sedate"
label(data$convulsions_admission)="Convulsions"
label(data$gcs_yeux_admission)="Score de Glasgow à ladmission - Yeux"
label(data$gcs_verbal_admission)="Score de Glasgow à ladmission - Verbal"
label(data$gcs_moteur_admission)="Score de Glasgow à ladmission - Moteur"
label(data$gcs_total_admission)="Score de Glasgow total à ladmission"
label(data$gcs_yeux_pire_24_heures)="Score de Glasgow le plus bas des 24 premières heures (en dehors de lintubation) - Yeux"
label(data$gcs_verbal_pire_24_heures)="Score de Glasgow le plus bas des 24 premières heures (en dehors de lintubation)  - Verbal"
label(data$gcs_moteur_pire_24_heures)="Score de Glasgow le plus bas des 24 premières heures (en dehors de lintubation)  - Moteur"
label(data$gcs_total_pire_24_heures)="Score de Glasgow total le plus bas des 24 premières heures (en dehors de lintubation) "
label(data$pupille_gauche_admission)="Pupille gauche"
label(data$pupille_droite_admission)="Pupille droite"
label(data$react_pupille_g)="Reactivite pupillaire gauche"
label(data$react_pupille_d)="Reactivite pupillaire droite"
label(data$dtc_admission)="Doppler transcraniens faits"
label(data$dtc_g_admission)="Fenêtre gauche disponible"
label(data$dtc_g_vs_admission)="Vitesse systolique (cm/s)"
label(data$dtc_g_vd_admission)="Vitesse diastolique (cm/s)"
label(data$dtc_g_ip_admission)="Index de pulsatilite"
label(data$dtc_d_admission)="Fenêtre droite disponible"
label(data$dtc_d_vs_admission)="Vitesse systolique (cm/s)"
label(data$dtc_d_vd_admission)="Vitesse diastolique (cm/s)"
label(data$dtc_d_ip_admission)="Index de pulsatilite"
label(data$imagerie_rea_24_heures)="Imagerie dans le centre dinclusion dans les 24 premieres heures"
label(data$imagerie_rea_type___1)="Type dimagerie (choice=TDM)"
label(data$imagerie_rea_type___2)="Type dimagerie (choice=IRM)"
label(data$imagerie_rea_injection)="Imagerie injectee"
label(data$osmo_rea)="Osmotherapie"
label(data$osmo_rea_indication)="Indication de losmotherapie"
label(data$osmo_rea_type)="Solute dosmotherapie"
label(data$osmo_rea_efficace)="Regression de lanomalie pupillaire apres losmotherapie ?"
label(data$intubation_rea)="Intubation"
label(data$etco2_rea)="Premier EtCO2 apres intubation (mmHg)"
label(data$norad_rea)="Noradrenaline"
label(data$norad_rea_indication)="Indication de la Noradrenaline"
label(data$diagnostic)="Diagnostic retenu"
label(data$prise_en_charge_initiale_complete)="Complete?"
label(data$mecanisme_trauma)="Mecanisme du traumatisme"
label(data$trauma_chute_6m)="Chute de plus de 6 mètres ?"
label(data$trauma_penetrant)="Trauma penetrant"
label(data$mecanisme_trauma_texte)="Autre : renseignez le mécanisme du trauma"
label(data$trauma_lesions_scan___1)="Lesions scannographiques  Noter les pires lesions des premieres 24 heures (choice=Hematome sous-dural)"
label(data$trauma_lesions_scan___2)="Lesions scannographiques  Noter les pires lesions des premieres 24 heures (choice=Hematome extra-dural)"
label(data$trauma_lesions_scan___3)="Lesions scannographiques  Noter les pires lesions des premieres 24 heures (choice=Hematome intra-parenchymateux)"
label(data$trauma_lesions_scan___4)="Lesions scannographiques  Noter les pires lesions des premieres 24 heures (choice=Petechie.s parenchymateuse.s)"
label(data$trauma_lesions_scan___5)="Lesions scannographiques  Noter les pires lesions des premieres 24 heures (choice=Hemorragie meningee)"
label(data$trauma_lesions_scan___6)="Lesions scannographiques  Noter les pires lesions des premieres 24 heures (choice=Hydrocephalie)"
label(data$trauma_lesions_scan___7)="Lesions scannographiques  Noter les pires lesions des premieres 24 heures (choice=Contusion.s parenchymateuse.s)"
label(data$tcdb_scan_classe)="Classification scannographique des lesions Renseigner un chiffre de 1 à 6 Coter les pires lesions des 24 premieres heures avant chirurgie 1 : normal 2 : lesions diffuses sans compression 3 : lesions diffuses avec compression (oedeme) 4 : lesions diffuses avec déviation > 5mm 5 : lesion(s) focale(s) (> 25 cm3) evacuees chirurgicalement 6 : lesion(s) focale(s) (> 25 cm3) non evacuees chirurgicalement"
label(data$trauma_vasospasme)="Vasospasme confirme a larteriographie"
label(data$trauma_lesions_associees)="Lésions traumatiques extra-crâniennes associées ?"
label(data$trauma_hemorragie_intervention)="Lésion hémorragique nécessitant un geste hémostatique ?"
label(data$trauma_type_hemostase___1)="Type dintervention hémostatique (choice=Radiologie interventionnelle)"
label(data$trauma_type_hemostase___2)="Type dintervention hémostatique (choice=Chirurgie)"
label(data$trauma_transfusion_2_cgr)="Transfusion > 2 CGR dans les 24 premières heures ?"
label(data$trauma_medullaire)="Traumatisme médullaire associée ?"
label(data$traumatisme_cranien_complete)="Complete?"
label(data$hsa_perte_conscience)="Perte de connaissance initiale ?"
label(data$hsa_wfns)="Echelle WFNS   GradeGCSDéficit moteur 115Non 213-14Non 313-14Oui 47-12Oui ou Non 53-6Oui ou Non "
label(data$hsa_fisher_modifiee)="Echelle de Fisher modifiée (0-4)   GradeImagerie 0Pas dHSA visible 1HSA minime, pas dHIV 2HSA minime, HIV presente 3HSA importante, pas dHIV bilaterale 4HSA importante, HIV bilaterale   HSA importante : une citerne ou une scissure totalement inondee HIV bilaterale : les deux ventricules lateraux"
label(data$hsa_anevrysme)="Anevrysme"
label(data$hsa_anevrysme_mav)="Anevrysme dans le cadre dune MAV"
label(data$hsa_anevrysme_localisation)="Localisation de lanevrysme  En cas danévrysmes multiples, ne renseigner que celui à lorigine de lHSA."
label(data$hsa_anevrysme_cote)="Lateralite de lanevrysme"
label(data$hsa_anevrysme_traitement)="Traitement de lanevrysme"
label(data$hsa_anevrysme_traitement_type)="Type de traitement de lanevrysme"
label(data$hsa_anevrysme_complication)="Complication ?"
label(data$hsa_anevrysme_compli_type___1)="Type de complication (choice=Débord de coil)"
label(data$hsa_anevrysme_compli_type___2)="Type de complication (choice=Occlusion vasculaire)"
label(data$hsa_anevrysme_compli_type___3)="Type de complication (choice=Dissection)"
label(data$hsa_anevrysme_compli_type___4)="Type de complication (choice=Rupture)"
label(data$hsa_anevrysme_compli_type___5)="Type de complication (choice=Resaignement)"
label(data$hsa_anevrysme_traitement_heure)="Date du traitement de lanevrysme"
label(data$hsa_anevrysme_nimodipine)="Traitement par Nimodipine"
label(data$hsa_anevrysme_statine)="Traitement par statine"
label(data$hsa_dysfonction_cardiaque)="Dysfonction cardiaque aigue ?"
label(data$hsa_fevg)="Fraction dejection ventriculaire gauche minimale"
label(data$hsa_dysf_card_inotrope)="Inotrope suite à la dysfonction cardiaque"
label(data$hsa_vasospasme)="Vasospasme"
label(data$hsa_vasospasme_dg_date)="Date du diagnostic du vasospasme"
label(data$hsa_vasospasme_dg_type___1)="Diagnostic du vasospasme  (plusieurs réponses possibles) (choice=Deficit neurologique)"
label(data$hsa_vasospasme_dg_type___2)="Diagnostic du vasospasme  (plusieurs réponses possibles) (choice=Doppler transcranien)"
label(data$hsa_vasospasme_dg_type___3)="Diagnostic du vasospasme  (plusieurs réponses possibles) (choice=TDM injectee)"
label(data$hsa_vasospasme_dg_type___4)="Diagnostic du vasospasme  (plusieurs réponses possibles) (choice=Arteriographie)"
label(data$hsa_vasospasme_dtc_vm)="Vitesse moyenne maximale de lACM au Doppler lors du diagnostic (cm/s)"
label(data$hsa_vasospasme_traitement)="Traitement du vasospasme"
label(data$hsa_vasospasme_traitement_type___1)="Type de traitement du vasospasme (plusieurs réponses possibles) (choice=Expansion volemique)"
label(data$hsa_vasospasme_traitement_type___2)="Type de traitement du vasospasme (plusieurs réponses possibles) (choice=Hypertension arterielle induite)"
label(data$hsa_vasospasme_traitement_type___3)="Type de traitement du vasospasme (plusieurs réponses possibles) (choice=Inodilatateur (milrinone))"
label(data$hsa_vasospasme_traitement_type___4)="Type de traitement du vasospasme (plusieurs réponses possibles) (choice=Inotrope (dobutamine))"
label(data$hsa_vasospasme_traitement_type___5)="Type de traitement du vasospasme (plusieurs réponses possibles) (choice=Dilatation chimique interventionnelle)"
label(data$hsa_vasospasme_traitement_type___6)="Type de traitement du vasospasme (plusieurs réponses possibles) (choice=Dilatation mecanique interventionnelle)"
label(data$hsa_vasospasme_territoire___1)="Territoire spasme (choice=Territoire anterieur)"
label(data$hsa_vasospasme_territoire___2)="Territoire spasme (choice=Territoire sylvien)"
label(data$hsa_vasospasme_territoire___3)="Territoire spasme (choice=Territoire posterieur)"
label(data$hsa_vasospasme_lateralite___1)="Lateralite du vasopasme (choice=Gauche)"
label(data$hsa_vasospasme_lateralite___2)="Lateralite du vasopasme (choice=Droit)"
label(data$hemorragie_meningee_complete)="Complete?"
label(data$hip_cause)="Cause de lhemorragie intracerebrale"
label(data$hip_cause_autre)="Cause autre de lAVC hémorragique"
label(data$hiv)="Hemorragie intraventriculaire associee ?"
label(data$hip_localisation___1)="Localisation de lhématome ? (choice=Supra-tentorielle)"
label(data$hip_localisation___2)="Localisation de lhématome ? (choice=Infra-tentorielle)"
label(data$hip_localisation___3)="Localisation de lhématome ? (choice=Profond)"
label(data$hip_localisation___4)="Localisation de lhématome ? (choice=Lobaire)"
label(data$hip_taille_30ml)="Taille de lhematome > 30mL ?  Formule de calcul :  A x B x C / 2 "
label(data$hip_taille_ml)="Volume de lhématome en mL"
label(data$accident_vasculaire_cerebral_hemorragique_complete)="Complete?"
label(data$aic_localisation___0)="Territoire de laccident vasculaire ischemique (choice=Carotidien)"
label(data$aic_localisation___1)="Territoire de laccident vasculaire ischemique (choice=Anterieur)"
label(data$aic_localisation___2)="Territoire de laccident vasculaire ischemique (choice=Sylvien superficiel)"
label(data$aic_localisation___3)="Territoire de laccident vasculaire ischemique (choice=Sylvien profond)"
label(data$aic_localisation___4)="Territoire de laccident vasculaire ischemique (choice=Posterieur)"
label(data$aic_localisation___5)="Territoire de laccident vasculaire ischemique (choice=Cerebelleux)"
label(data$aic_localisation___6)="Territoire de laccident vasculaire ischemique (choice=Tronc cerebral)"
label(data$aic_localisation___7)="Territoire de laccident vasculaire ischemique (choice=Autre)"
label(data$aic_localisation_texte)="Autre : specifiez la localisation"
label(data$aic_lateralite___1)="Lateralite de laccident vasculaire ischemique (choice=Gauche)"
label(data$aic_lateralite___2)="Lateralite de laccident vasculaire ischemique (choice=Droit)"
label(data$aic_cause)="Cause de laccident vasculaire cérébral ischémique"
label(data$aic_cause_autre)="Autre : spécifiez la cause de lAIC"
label(data$aic_nihss)="Score NIHSS initial (0 - 42)"
label(data$aic_thrombolyse)="Thrombolyse"
label(data$aic_thrombectomie)="Thrombectomie"
label(data$thrombectomie_anesthesie)="Modalite danesthesie"
label(data$aic_thrombectomie_heure_ponction)="Heure de la ponction arterielle"
label(data$aic_thrombectomie_revasc)="Repermeabilisation ?"
label(data$aic_thrombectomie_tici)="Score TICI (0-3)  Se référer au CR de NRI pour obtenir le score"
label(data$aic_thrombectomie_revasc_heure)="Heure de la repermeabilisation"
label(data$aic_stent)="Pose de stent"
label(data$aic_stent_localisation___1)="Localisation du stent (choice=Artère cérébrale moyenne)"
label(data$aic_stent_localisation___2)="Localisation du stent (choice=Artère carotide)"
label(data$aic_stent_localisation___3)="Localisation du stent (choice=Tronc basilaire)"
label(data$aic_stent_localisation___4)="Localisation du stent (choice=Artère vertébrale)"
label(data$aic_stent_localisation___5)="Localisation du stent (choice=Autre)"
label(data$aic_stent_loca_autre)="Spécifiez la localisation"
label(data$aic_nri_complication)="Complications de la NRI ?"
label(data$aic_nri_complication_type___1)="Type de complication (choice=Migration embolique)"
label(data$aic_nri_complication_type___2)="Type de complication (choice=Occlusion dun stent)"
label(data$aic_nri_complication_type___3)="Type de complication (choice=Hemorragie per-procedure)"
label(data$aic_nri_complication_type___4)="Type de complication (choice=Hemorragie au point de ponction)"
label(data$aic_nri_complication_type___5)="Type de complication (choice=Spasme arteriel)"
label(data$aic_nri_complication_type___6)="Type de complication (choice=Troubles du rythme cardiaque)"
label(data$aic_nri_complication_type___7)="Type de complication (choice=Faux-anevrysme arteriel)"
label(data$aic_complication)="Complication de lAIC ?"
label(data$aic_complication_type___1)="Type de complication (choice=Récidive ischémique)"
label(data$aic_complication_type___2)="Type de complication (choice=Transformation hémorragique)"
label(data$accident_vasculaire_ischemique_complete)="Complete?"
label(data$inm_localisation___1)="Localisation de linfection ? (choice=Empyème extradural ou sous-dural)"
label(data$inm_localisation___2)="Localisation de linfection ? (choice=Méningite)"
label(data$inm_localisation___3)="Localisation de linfection ? (choice=Ventriculite)"
label(data$inm_localisation___4)="Localisation de linfection ? (choice=Encéphalite)"
label(data$inm_localisation___5)="Localisation de linfection ? (choice=Abcès cérébral)"
label(data$inm_communautaire)="Infection communautaire ?   Rappel : une infection associée aux soins est développée 48 heures après ladmission à lhôpital ou dans les suites dune procédure diagnostique ou thérapeutique. Dans ce cas, répondre Non à la question."
label(data$inm_porte_entree)="Porte dentrée ?"
label(data$inm_porte_entree_texte)="Précisez la porte dentrée"
label(data$inm_prelevement)="Prélèvements bactériologiques ?"
label(data$inm_prelevement_date)="Date des premiers prélèvements"
label(data$inm_prelevement_type)="Type de prélèvement"
label(data$inm_prelevement_lactate)="Dosage de la lactatorachie ?"
label(data$inm_prelevement_lactatorachie)="Lactatorachie (mmol/L)"
label(data$inm_bacteriemie)="Bactériémie associée ?"
label(data$inm_bacteriemie_date)="Date de la bactériémie"
label(data$inm_nb_microbes)="Nombre de microbes identifiés"
label(data$inm_microbe_1)="Microbe N°1"
label(data$inm_microbe_2)="Microbe N°2"
label(data$inm_microbe_3)="Microbe N°3"
label(data$inm_microbe_4)="Microbe N°4"
label(data$inm_microbe_5)="Microbe N°5"
label(data$inm_ttt_proba_mono)="Traitement anti-infectieux empirique en monothérapie ?  Ce champ napparaît que sil existe un micro-organisme dans les prélèvements et donc suppose que le traitement anti-infectieux est entrepris.  Il faut donc répondre Non à cette question seulement si le traitement est une bi ou une trithérapie. "
label(data$inm_ttt_proba_mol_1)="Traitement anti-infectieux empirique  Molécule 1"
label(data$inm_ttt_proba_mol_1_txt)="Traitement anti-infectieux empirique  Molécule 1  Autre : renseignez la DCI"
label(data$inm_ttt_proba_mol_2)="Traitement anti-infectieux empirique  Molécule 2"
label(data$inm_ttt_proba_mol_2_txt)="Traitement anti-infectieux empirique  Molécule 2  Autre : renseignez la DCI"
label(data$inm_ttt_docu_mono)="Traitement anti-infectieux documenté en monothérapie ?  Ce champ napparaît que sil existe un micro-organisme dans les prélèvements et donc suppose que le traitement anti-infectieux est entrepris.  Il faut donc répondre Non à cette question seulement si le traitement est une bi ou une trithérapie."
label(data$inm_ttt_docu_mol_1)="Traitement anti-infectieux documenté  Molécule 1"
label(data$inm_ttt_docu_mol_1_txt)="Traitement anti-infectieux documenté  Molécule 1  Autre : renseignez la DCI"
label(data$inm_ttt_docu_mol_2)="Traitement anti-infectieux documenté  Molécule 2"
label(data$inm_ttt_docu_mol_2_txt)="Traitement anti-infectieux documenté  Molécule 2  Autre : renseignez la DCI"
label(data$inm_chirurgie)="Prise en charge chirurgicale de linfection ?"
label(data$inm_evolution)="Evolution de linfection ?  Loption Autre infection neuro-méningée doit être utilisée pour décrire une infection neuro-méningée supplémentaire sans rapport et compliquant la première.  Exemple : infection de DVE dans les suites dun empyème sous-dural"
label(data$infection_neuromeningee_complete)="Complete?"
label(data$tumeur_atcd)="Lésion tumorale préalable connue ?"
label(data$tumeur_localisation___1)="Localisation de la tumeur (choice=Hémisphère droit)"
label(data$tumeur_localisation___2)="Localisation de la tumeur (choice=Hémisphère gauche)"
label(data$tumeur_localisation___3)="Localisation de la tumeur (choice=Bihémisphérique)"
label(data$tumeur_localisation___4)="Localisation de la tumeur (choice=Tronc cérébral)"
label(data$tumeur_localisation___5)="Localisation de la tumeur (choice=Tronc cérébral)"
label(data$tumeur_localisation___6)="Localisation de la tumeur (choice=Cervelet)"
label(data$tumeur_localisation___7)="Localisation de la tumeur (choice=Système ventriculaire)"
label(data$tumeur_cause_degr___1)="Cause de la dégradation neurologique (choice=HTIC)"
label(data$tumeur_cause_degr___2)="Cause de la dégradation neurologique (choice=Hydrocéphalie obstructive)"
label(data$tumeur_cause_degr___3)="Cause de la dégradation neurologique (choice=Etat de mal épileptique)"
label(data$tumeur_cause_degr___4)="Cause de la dégradation neurologique (choice=Saignement tumoral)"
label(data$tumeur_cause_degr___5)="Cause de la dégradation neurologique (choice=Autre)"
label(data$tumeur_cause_degr_txt)="Précisez la dégradation neurologoque"
label(data$tumeur_imagerie_diag___1)="Modalité diagnostique (choice=TDM)"
label(data$tumeur_imagerie_diag___2)="Modalité diagnostique (choice=IRM)"
label(data$tumeur_imagerie_diag___3)="Modalité diagnostique (choice=TEP-TDM)"
label(data$tumeur_imagerie_diag_inj)="Imagerie diagnostique injectée ?"
label(data$tumeur_confirm_diag)="Confirmation diagnostique ?"
label(data$tumeur_confirm_diag_proc___1)="Procédure diagnostique de confirmation (choice=Ponction lombaire)"
label(data$tumeur_confirm_diag_proc___2)="Procédure diagnostique de confirmation (choice=Biopsie intracrânienne)"
label(data$tumeur_confirm_diag_proc___3)="Procédure diagnostique de confirmation (choice=Biopsie autre)"
label(data$tumeur_anapath)="Analyse anatomopathologique ?"
label(data$tumeur_anapath_benin)="Tumeur bénigne ?"
label(data$tumeur_anapath_type___1)="Typage tumoral (choice=Méningiome)"
label(data$tumeur_anapath_type___2)="Typage tumoral (choice=Neurocytome)"
label(data$tumeur_anapath_type___3)="Typage tumoral (choice=Gliome)"
label(data$tumeur_anapath_type___4)="Typage tumoral (choice=Glioblastome)"
label(data$tumeur_anapath_type___5)="Typage tumoral (choice=Astrocytome)"
label(data$tumeur_anapath_type___6)="Typage tumoral (choice=Lymphome cérébral)"
label(data$tumeur_anapath_type___7)="Typage tumoral (choice=Autre)"
label(data$tumeur_anapath_type_txt)="Précisez le type de la tumeur"
label(data$tumeur_ttt_urgent)="Thérapeutique en urgence ?"
label(data$tumeur_ttt_type___1)="Type(s) de thérapeutique (choice=Corticothérapie)"
label(data$tumeur_ttt_type___2)="Type(s) de thérapeutique (choice=Chimiothérapie)"
label(data$tumeur_ttt_type___3)="Type(s) de thérapeutique (choice=Immunothérapie)"
label(data$tumeur_ttt_type___4)="Type(s) de thérapeutique (choice=Traitement antiépileptique)"
label(data$tumeur_ttt_type___5)="Type(s) de thérapeutique (choice=Dérivation ventriculaire)"
label(data$tumeur_ttt_type___6)="Type(s) de thérapeutique (choice=Autre)"
label(data$tumeur_ttt_type_txt)="Précisez la thérapeutique"
label(data$tumeur_ttt_exer)="Exérèse chirurgicale ?"
label(data$tumeur_ttt_exer_compl)="Exérèse complète ?"
label(data$tumeur_ttt_adj)="Thérapeutique adjuvante ?"
label(data$tumeur_adj_ttt_adj_type___1)="Type de thérapeutique adjuvante (choice=Neuroradiologie interventionnelle)"
label(data$tumeur_adj_ttt_adj_type___2)="Type de thérapeutique adjuvante (choice=Radiothérapie)"
label(data$tumeur_adj_ttt_adj_type___3)="Type de thérapeutique adjuvante (choice=Chimiothérapie)"
label(data$tumeur_adj_ttt_adj_type___4)="Type de thérapeutique adjuvante (choice=Immunothérapie)"
label(data$tumeur_complete)="Complete?"
label(data$capteur_pic)="Pose dun capteur de PIC"
label(data$capteur_pic_date)="Date de la pose du capteur de PIC"
label(data$htic)="Hypertension intracranienne ?  Repondre oui si lhypertension intracranienne a ete a lorigine dun traitement medical ou chirurgical."
label(data$htic_pic_max)="Valeur maximale de la PIC (premiere semaine)  Ne prendre que les valeurs au repos et maintenues plus de 5 minutes"
label(data$htic_traitement___1)="Traitement de lhypertension intracrânienne (choice=Sedation)"
label(data$htic_traitement___2)="Traitement de lhypertension intracrânienne (choice=Curarisation)"
label(data$htic_traitement___3)="Traitement de lhypertension intracrânienne (choice=Normothermie active)"
label(data$htic_traitement___4)="Traitement de lhypertension intracrânienne (choice=Hypothermie therapeutique)"
label(data$htic_traitement___5)="Traitement de lhypertension intracrânienne (choice=Derivation ventriculaire externe)"
label(data$htic_traitement___11)="Traitement de lhypertension intracrânienne (choice=Dérivation lombaire externe)"
label(data$htic_traitement___6)="Traitement de lhypertension intracrânienne (choice=Craniectomie decompressive)"
label(data$htic_traitement___7)="Traitement de lhypertension intracrânienne (choice=Suppression metabolique)"
label(data$htic_traitement___8)="Traitement de lhypertension intracrânienne (choice=Osmotherapie)"
label(data$htic_traitement___9)="Traitement de lhypertension intracrânienne (choice=Hypocapnie permissive)"
label(data$htic_traitement___10)="Traitement de lhypertension intracrânienne (choice=Autre)"
label(data$htic_traitement_hypnotique___1)="Type de sedatif utilise (choice=Midazolam)"
label(data$htic_traitement_hypnotique___2)="Type de sedatif utilise (choice=Ketamine)"
label(data$htic_traitement_hypnotique___3)="Type de sedatif utilise (choice=Propofol)"
label(data$htic_traitement_hypnotique___4)="Type de sedatif utilise (choice=Thiopental)"
label(data$htic_traitement_curare___1)="Type de curare utilise (choice=Atracurium)"
label(data$htic_traitement_curare___2)="Type de curare utilise (choice=Cisatracurium)"
label(data$htic_traitement_curare___3)="Type de curare utilise (choice=Rocuronium)"
label(data$htic_traitement_temp_cible)="Temperature cible"
label(data$htic_traitement_dve_date)="Date de pose de la derivation ventriculaire externe"
label(data$htic_traitement_craniec_date)="Date de la craniectomie decompressive"
label(data$htic_traitement_burst_type)="Type dhypnotique utilise pour la suppression metabolique"
label(data$htic_traitement_texte)="Autre : specifiez le type de traitement"
label(data$htic_traitement_autre_txt)="Renseigner la thérapeutique de lHTIC"
label(data$corticoides)="Traitement par corticoïdes ?"
label(data$monitorage_tcd)="Doppler transcraniens en reanimation  Oui si au moins un Doppler par jour pendant les 5 premiers jours de lhospitalisation en reanimation"
label(data$dtc_bf_rea)="Backflow en réanimation ?"
label(data$tcd_vd_min)="Velocite diastolique minimale (cm/s)"
label(data$dtc_ip_max)="IP maximal"
label(data$capteur_ptio2)="Pose dun capteur de PtiO2"
label(data$ptio2_min)="Valeur minimale de la PtiO2"
label(data$capteur_svjo2)="Pose dun capteur de SvjO2"
label(data$pvjo2_min)="Valeur minimale de PvjO2 (mmHg)"
label(data$microdialyse)="Microdialyse"
label(data$eeg_continu)="EEG continu"
label(data$neurochirurgie)="Neurochirurgie"
label(data$neurochirurgie_date)="Date de la neurochirurgie"
label(data$neurochirurgie_type___1)="Type de neurochirurgie (choice=Évacuation dhématome)"
label(data$neurochirurgie_type___2)="Type de neurochirurgie (choice=Dérivation ventriculaire externe)"
label(data$neurochirurgie_type___7)="Type de neurochirurgie (choice=Dérivation ventriculo-péritonéale ou atriale)"
label(data$neurochirurgie_type___3)="Type de neurochirurgie (choice=Clipping anévrysmal)"
label(data$neurochirurgie_type___4)="Type de neurochirurgie (choice=Craniectomie décompressive hémisphérique)"
label(data$neurochirurgie_type___6)="Type de neurochirurgie (choice=Craniectomie décompressive de fosse postérieure)"
label(data$neurochirurgie_type___5)="Type de neurochirurgie (choice=Autre)"
label(data$neurochirurgie_type_texte)="Autre : specifiez le type de neurochirurgie"
label(data$neurochirurgie_nombre)="Nombre total de neurochirurgies"
label(data$epilepsie)="Epilepsie"
label(data$epilepsie_edme)="Etat de mal epileptique"
label(data$epilepsie_edme_refrac)="Etat de mal refractaire"
label(data$epilepsie_traitement___1)="Traitement anti-epileptique (choice=Fosphenytoïne (DIHYDAN))"
label(data$epilepsie_traitement___2)="Traitement anti-epileptique (choice=Levetiracetam (KEPPRA))"
label(data$epilepsie_traitement___3)="Traitement anti-epileptique (choice=Benzodiazepines (VALIUM, RIVOTRIL))"
label(data$epilepsie_traitement___4)="Traitement anti-epileptique (choice=Valproate de sodium (DEPAKINE))"
label(data$epilepsie_traitement___5)="Traitement anti-epileptique (choice=Propofol (DIPRIVAN))"
label(data$epilepsie_traitement___6)="Traitement anti-epileptique (choice=Thiopental (PENTOTHAL))"
label(data$epilepsie_traitement___7)="Traitement anti-epileptique (choice=Autre)"
label(data$ventilation_rea)="Ventilation mecanique en reanimation"
label(data$ventilation_rea_duree)="Duree totale de la ventilation mecanique (en jours)"
label(data$infection_rea)="Infection en reanimation"
label(data$infection_rea_precoce)="Infection précoce (< 5 jours) ?"
label(data$infection_rea_precoce_inh)="Pneumonie dinhalation"
label(data$infection_rea_precoce_texte)="Infection precoce : precisez le type dinfection"
label(data$infection_rea_tardive)="Infection tardive (>5 jours) ?"
label(data$infection_rea_tard_type___1)="Infection tardive (choice=Pneumonie)"
label(data$infection_rea_tard_type___2)="Infection tardive (choice=Infection de site operatoire)"
label(data$infection_rea_tard_type___3)="Infection tardive (choice=Meningo-ventriculite)"
label(data$infection_rea_tard_type___4)="Infection tardive (choice=Infection de catheter)"
label(data$infection_rea_tard_type___5)="Infection tardive (choice=Autre)"
label(data$infection_rea_tard_texte)="Autre : precisez le type dinfection "
label(data$infection_rea_choc)="Choc septique"
label(data$sedation_rea)="Sedation en reanimation"
label(data$sedation_rea_date)="Date de debut de la sedation"
label(data$sedation_rea_duree)="Duree totale de la sedation (en jours)"
label(data$sedation_rea_type___1)="Type de sedation (choice=Midazolam)"
label(data$sedation_rea_type___2)="Type de sedation (choice=Ketamine)"
label(data$sedation_rea_type___3)="Type de sedation (choice=Propofol)"
label(data$sedation_rea_type___4)="Type de sedation (choice=Dexmedetomidine)"
label(data$sedation_rea_type___5)="Type de sedation (choice=Clonidine)"
label(data$trachetomie)="Tracheotomie"
label(data$tbes_deglutition)="Troubles de la déglutition"
label(data$gpe)="Gastrostomie percutanee"
label(data$na_min)="Natremie minimale (mmol/L)"
label(data$na_max)="Natremie maximale (mmol/L)"
label(data$lata)="Limitation des therapeutiques actives"
label(data$ata)="Arret des therapeutiques actives"
label(data$deces_rea)="Deces en reanimation"
label(data$deces_date)="Date de deces"
label(data$edme)="Passage en etat de mort encephalique"
label(data$edme_date)="Date de passage en etat de mort encephalique"
label(data$sortie_date)="Date de sortie de réanimation"
label(data$pmo)="Prelevement dorgane"
label(data$pmo_maastricht)="Contexte du prelevement"
label(data$edme_confirmation)="Modalite de confirmation de letat de mort encephalique"
label(data$prise_en_charge_en_reanimation_complete)="Complete?"
label(data$sortie_rea)="Destination a la sortie de la reanimation"
label(data$sortie_rea_texte)="Destination a la sortie de la reanimation Autre : precisez le lieu de sortie"
label(data$ouverture_yeux)="Ouverture des yeux ?"
label(data$ouverture_yeux_date)="Date douverture des yeux "
label(data$etat_conscience)="Etat de conscience a la sortie ?"
label(data$crsr)="Evaluation par CRSR ?"
label(data$crsr_premier)="Premier score CRSR disponible (Min 0 ; Max 23)"
label(data$crsr_premier_date)="Date du premier CRSR"
label(data$crsr_sortie)="Score CRSR à la sortie de réanimation"
label(data$gcs_sortie)="Score de Glasgow à la sortie de réanimation"
label(data$gos_sortie)="Score  GOS à la sortie de reanimation (Min 1 ; Max 5)  "
label(data$gose_sortie)="Score  GOS-E à la sortie de reanimation (Min 1 ; Max 8)"
label(data$goat_sortie)="Score  de GOAT à la sortie de reanimation (Min 0 ; Max 100)  "
label(data$drs_sortie)="Score DRS à la sortie de réanimation [0-29]"
label(data$eval_distance)="Evaluation a distance"
label(data$evaluation_distance_date)="Date de la dernière évaluation"
label(data$evaluation_distance_deces)="Patient vivant lors de levaluation"
label(data$evaluation_distance_rad)="Retour à domicile"
label(data$evaluation_distance_pec_spe)="Prise en charge en rééducation spécialisée"
label(data$eval_distance_score___1)="Score(s) utilise(s) pour levaluation  mRs : Modified Ranking Scale GOS : Glasgow Outcome Scale GOSE : Glasgow Outcome Scale Extended DRS : Disability Rating Scale RiverMead : RiverMead Mobility Score (choice=mRS)"
label(data$eval_distance_score___5)="Score(s) utilise(s) pour levaluation  mRs : Modified Ranking Scale GOS : Glasgow Outcome Scale GOSE : Glasgow Outcome Scale Extended DRS : Disability Rating Scale RiverMead : RiverMead Mobility Score (choice=GOS)"
label(data$eval_distance_score___2)="Score(s) utilise(s) pour levaluation  mRs : Modified Ranking Scale GOS : Glasgow Outcome Scale GOSE : Glasgow Outcome Scale Extended DRS : Disability Rating Scale RiverMead : RiverMead Mobility Score (choice=GOSE)"
label(data$eval_distance_score___3)="Score(s) utilise(s) pour levaluation  mRs : Modified Ranking Scale GOS : Glasgow Outcome Scale GOSE : Glasgow Outcome Scale Extended DRS : Disability Rating Scale RiverMead : RiverMead Mobility Score (choice=DRS)"
label(data$eval_distance_score___6)="Score(s) utilise(s) pour levaluation  mRs : Modified Ranking Scale GOS : Glasgow Outcome Scale GOSE : Glasgow Outcome Scale Extended DRS : Disability Rating Scale RiverMead : RiverMead Mobility Score (choice=Rivermead)"
label(data$eval_distance_score___4)="Score(s) utilise(s) pour levaluation  mRs : Modified Ranking Scale GOS : Glasgow Outcome Scale GOSE : Glasgow Outcome Scale Extended DRS : Disability Rating Scale RiverMead : RiverMead Mobility Score (choice=Autre)"
label(data$eval_distance_score_mrs)="Score mRs"
label(data$eval_distance_score_gos)="Score  GOS   "
label(data$eval_distance_score_gose)="Score GOS-E"
label(data$eval_distance_score_drs)="Score DRS"
label(data$eval_distance_score_rm)="Score Rivermead"
label(data$eval_distance_score_texte)="Score utilise pour levaluation Autre : precisez le score et sa valeur"
label(data$donnees_a_la_suite_de_la_reanimation_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$criteres_inclusion___1.factor = factor(data$criteres_inclusion___1,levels=c("0","1"))
data$criteres_inclusion___2.factor = factor(data$criteres_inclusion___2,levels=c("0","1"))
data$criteres_inclusion___3.factor = factor(data$criteres_inclusion___3,levels=c("0","1"))
data$criteres_inclusion___4.factor = factor(data$criteres_inclusion___4,levels=c("0","1"))
data$centre_inclusion.factor = factor(data$centre_inclusion,levels=c("BCT","LRB","BJN","PS","HM"))
data$traumabase.factor = factor(data$traumabase,levels=c("1","0"))
data$sex.factor = factor(data$sex,levels=c("1","2","3"))
data$femme_peripartum.factor = factor(data$femme_peripartum,levels=c("1","0"))
data$more_than_2_years_old.factor = factor(data$more_than_2_years_old,levels=c("1","0"))
data$ieyn.factor = factor(data$ieyn,levels=c("1","0"))
data$donnes_dmographiques_aphp_complete.factor = factor(data$donnes_dmographiques_aphp_complete,levels=c("0","1","2"))
data$hta.factor = factor(data$hta,levels=c("1","0","2"))
data$hta_traitement.factor = factor(data$hta_traitement,levels=c("1","0"))
data$diabete.factor = factor(data$diabete,levels=c("1","2","3","4","5"))
data$tabac.factor = factor(data$tabac,levels=c("1","2","3"))
data$intox_chronique___1.factor = factor(data$intox_chronique___1,levels=c("0","1"))
data$intox_chronique___2.factor = factor(data$intox_chronique___2,levels=c("0","1"))
data$intox_chronique___3.factor = factor(data$intox_chronique___3,levels=c("0","1"))
data$intox_chronique___4.factor = factor(data$intox_chronique___4,levels=c("0","1"))
data$intox_chronique___5.factor = factor(data$intox_chronique___5,levels=c("0","1"))
data$intox_aigue___1.factor = factor(data$intox_aigue___1,levels=c("0","1"))
data$intox_aigue___2.factor = factor(data$intox_aigue___2,levels=c("0","1"))
data$intox_aigue___3.factor = factor(data$intox_aigue___3,levels=c("0","1"))
data$intox_aigue___4.factor = factor(data$intox_aigue___4,levels=c("0","1"))
data$intox_aigue___5.factor = factor(data$intox_aigue___5,levels=c("0","1"))
data$aap.factor = factor(data$aap,levels=c("1","0"))
data$aap_type___1.factor = factor(data$aap_type___1,levels=c("0","1"))
data$aap_type___2.factor = factor(data$aap_type___2,levels=c("0","1"))
data$aap_type___3.factor = factor(data$aap_type___3,levels=c("0","1"))
data$aap_type___4.factor = factor(data$aap_type___4,levels=c("0","1"))
data$aap_type___5.factor = factor(data$aap_type___5,levels=c("0","1"))
data$aap_type___6.factor = factor(data$aap_type___6,levels=c("0","1"))
data$aap_type___7.factor = factor(data$aap_type___7,levels=c("0","1"))
data$lateralite.factor = factor(data$lateralite,levels=c("1","2","3","4"))
data$anticoagulant.factor = factor(data$anticoagulant,levels=c("1","0"))
data$anticoagulant_type___1.factor = factor(data$anticoagulant_type___1,levels=c("0","1"))
data$anticoagulant_type___2.factor = factor(data$anticoagulant_type___2,levels=c("0","1"))
data$anticoagulant_type___3.factor = factor(data$anticoagulant_type___3,levels=c("0","1"))
data$anticoagulant_type___4.factor = factor(data$anticoagulant_type___4,levels=c("0","1"))
data$francophone.factor = factor(data$francophone,levels=c("1","0"))
data$isolement.factor = factor(data$isolement,levels=c("1","0"))
data$activite.factor = factor(data$activite,levels=c("1.","2.","3.","4.","5.","6.","7.","8.","9."))
data$antecedents_complete.factor = factor(data$antecedents_complete,levels=c("0","1","2"))
data$pas_inf_90.factor = factor(data$pas_inf_90,levels=c("1","0"))
data$spo2_inf_90.factor = factor(data$spo2_inf_90,levels=c("1","0"))
data$gcs_detaille.factor = factor(data$gcs_detaille,levels=c("1","0"))
data$gcs_yeux_initial.factor = factor(data$gcs_yeux_initial,levels=c("1","2","3","4"))
data$gcs_verbal_initial.factor = factor(data$gcs_verbal_initial,levels=c("1","2","3","4","5"))
data$gcs_moteur_initial.factor = factor(data$gcs_moteur_initial,levels=c("1","2","3","4","5","6"))
data$convulsions_initial.factor = factor(data$convulsions_initial,levels=c("1","0"))
data$pupille_gauche_admission_v2.factor = factor(data$pupille_gauche_admission_v2,levels=c("1","2","3"))
data$pupille_droite_admission_v2.factor = factor(data$pupille_droite_admission_v2,levels=c("1","2","3"))
data$dtc_initial_fait.factor = factor(data$dtc_initial_fait,levels=c("1","0"))
data$bf_dtc_initial.factor = factor(data$bf_dtc_initial,levels=c("1","0"))
data$bf_dtc_initial_cote___1.factor = factor(data$bf_dtc_initial_cote___1,levels=c("0","1"))
data$bf_dtc_initial_cote___2.factor = factor(data$bf_dtc_initial_cote___2,levels=c("0","1"))
data$dtc_g_initial.factor = factor(data$dtc_g_initial,levels=c("1","0"))
data$dtc_d_initial.factor = factor(data$dtc_d_initial,levels=c("1","0"))
data$imagerie_prerea.factor = factor(data$imagerie_prerea,levels=c("1","0"))
data$imagerie_prerea_type___1.factor = factor(data$imagerie_prerea_type___1,levels=c("0","1"))
data$imagerie_prerea_type___2.factor = factor(data$imagerie_prerea_type___2,levels=c("0","1"))
data$imagerie_prerea_injection.factor = factor(data$imagerie_prerea_injection,levels=c("1","0"))
data$malformation_vasculaire.factor = factor(data$malformation_vasculaire,levels=c("1","0"))
data$malformation_vasculaire_type___1.factor = factor(data$malformation_vasculaire_type___1,levels=c("0","1"))
data$malformation_vasculaire_type___2.factor = factor(data$malformation_vasculaire_type___2,levels=c("0","1"))
data$malformation_vasculaire_type___3.factor = factor(data$malformation_vasculaire_type___3,levels=c("0","1"))
data$osmo_prerea.factor = factor(data$osmo_prerea,levels=c("1","0"))
data$osmo_prerea_indication___1.factor = factor(data$osmo_prerea_indication___1,levels=c("0","1"))
data$osmo_prerea_indication___2.factor = factor(data$osmo_prerea_indication___2,levels=c("0","1"))
data$osmo_prerea_indication___3.factor = factor(data$osmo_prerea_indication___3,levels=c("0","1"))
data$osmo_prerea_type___1.factor = factor(data$osmo_prerea_type___1,levels=c("0","1"))
data$osmo_prerea_type___2.factor = factor(data$osmo_prerea_type___2,levels=c("0","1"))
data$osmo_prerea_efficace.factor = factor(data$osmo_prerea_efficace,levels=c("1","0"))
data$intubation_prerea.factor = factor(data$intubation_prerea,levels=c("1","0"))
data$etco2_dispo.factor = factor(data$etco2_dispo,levels=c("1","0"))
data$norad_prerea.factor = factor(data$norad_prerea,levels=c("1","0"))
data$norad_prerea_indication.factor = factor(data$norad_prerea_indication,levels=c("1","2"))
data$acr_prehosp.factor = factor(data$acr_prehosp,levels=c("1","0"))
data$prise_en_charge_avant_la_reanimation_complete.factor = factor(data$prise_en_charge_avant_la_reanimation_complete,levels=c("0","1","2"))
data$mode_admission.factor = factor(data$mode_admission,levels=c("1","2","3"))
data$admission_ggnc_pdses.factor = factor(data$admission_ggnc_pdses,levels=c("1","2","3"))
data$sedation_admission.factor = factor(data$sedation_admission,levels=c("1","0"))
data$convulsions_admission.factor = factor(data$convulsions_admission,levels=c("1","0"))
data$gcs_yeux_admission.factor = factor(data$gcs_yeux_admission,levels=c("1","2","3","4"))
data$gcs_verbal_admission.factor = factor(data$gcs_verbal_admission,levels=c("1","2","3","4","5"))
data$gcs_moteur_admission.factor = factor(data$gcs_moteur_admission,levels=c("1","2","3","4","5","6"))
data$gcs_yeux_pire_24_heures.factor = factor(data$gcs_yeux_pire_24_heures,levels=c("1","2","3","4"))
data$gcs_verbal_pire_24_heures.factor = factor(data$gcs_verbal_pire_24_heures,levels=c("1","2","3","4","5"))
data$gcs_moteur_pire_24_heures.factor = factor(data$gcs_moteur_pire_24_heures,levels=c("1","2","3","4","5","6"))
data$pupille_gauche_admission.factor = factor(data$pupille_gauche_admission,levels=c("1","2","3"))
data$pupille_droite_admission.factor = factor(data$pupille_droite_admission,levels=c("1","2","3"))
data$react_pupille_g.factor = factor(data$react_pupille_g,levels=c("1","0"))
data$react_pupille_d.factor = factor(data$react_pupille_d,levels=c("1","0"))
data$dtc_admission.factor = factor(data$dtc_admission,levels=c("1","0"))
data$dtc_g_admission.factor = factor(data$dtc_g_admission,levels=c("1","0"))
data$dtc_d_admission.factor = factor(data$dtc_d_admission,levels=c("1","0"))
data$imagerie_rea_24_heures.factor = factor(data$imagerie_rea_24_heures,levels=c("1","0"))
data$imagerie_rea_type___1.factor = factor(data$imagerie_rea_type___1,levels=c("0","1"))
data$imagerie_rea_type___2.factor = factor(data$imagerie_rea_type___2,levels=c("0","1"))
data$imagerie_rea_injection.factor = factor(data$imagerie_rea_injection,levels=c("1","0"))
data$osmo_rea.factor = factor(data$osmo_rea,levels=c("1","0"))
data$osmo_rea_indication.factor = factor(data$osmo_rea_indication,levels=c("1","2"))
data$osmo_rea_type.factor = factor(data$osmo_rea_type,levels=c("1","2"))
data$osmo_rea_efficace.factor = factor(data$osmo_rea_efficace,levels=c("1","0"))
data$intubation_rea.factor = factor(data$intubation_rea,levels=c("1","0"))
data$norad_rea.factor = factor(data$norad_rea,levels=c("1","0"))
data$norad_rea_indication.factor = factor(data$norad_rea_indication,levels=c("1","2"))
data$diagnostic.factor = factor(data$diagnostic,levels=c("1","2","3","4","5","6"))
data$prise_en_charge_initiale_complete.factor = factor(data$prise_en_charge_initiale_complete,levels=c("0","1","2"))
data$mecanisme_trauma.factor = factor(data$mecanisme_trauma,levels=c("1","2","3","4","5"))
data$trauma_chute_6m.factor = factor(data$trauma_chute_6m,levels=c("1","0"))
data$trauma_penetrant.factor = factor(data$trauma_penetrant,levels=c("1","0"))
data$trauma_lesions_scan___1.factor = factor(data$trauma_lesions_scan___1,levels=c("0","1"))
data$trauma_lesions_scan___2.factor = factor(data$trauma_lesions_scan___2,levels=c("0","1"))
data$trauma_lesions_scan___3.factor = factor(data$trauma_lesions_scan___3,levels=c("0","1"))
data$trauma_lesions_scan___4.factor = factor(data$trauma_lesions_scan___4,levels=c("0","1"))
data$trauma_lesions_scan___5.factor = factor(data$trauma_lesions_scan___5,levels=c("0","1"))
data$trauma_lesions_scan___6.factor = factor(data$trauma_lesions_scan___6,levels=c("0","1"))
data$trauma_lesions_scan___7.factor = factor(data$trauma_lesions_scan___7,levels=c("0","1"))
data$trauma_vasospasme.factor = factor(data$trauma_vasospasme,levels=c("1","0"))
data$trauma_lesions_associees.factor = factor(data$trauma_lesions_associees,levels=c("1","0"))
data$trauma_hemorragie_intervention.factor = factor(data$trauma_hemorragie_intervention,levels=c("1","0"))
data$trauma_type_hemostase___1.factor = factor(data$trauma_type_hemostase___1,levels=c("0","1"))
data$trauma_type_hemostase___2.factor = factor(data$trauma_type_hemostase___2,levels=c("0","1"))
data$trauma_transfusion_2_cgr.factor = factor(data$trauma_transfusion_2_cgr,levels=c("1","0"))
data$trauma_medullaire.factor = factor(data$trauma_medullaire,levels=c("1","0"))
data$traumatisme_cranien_complete.factor = factor(data$traumatisme_cranien_complete,levels=c("0","1","2"))
data$hsa_perte_conscience.factor = factor(data$hsa_perte_conscience,levels=c("1","0"))
data$hsa_anevrysme.factor = factor(data$hsa_anevrysme,levels=c("1","0"))
data$hsa_anevrysme_mav.factor = factor(data$hsa_anevrysme_mav,levels=c("1","0"))
data$hsa_anevrysme_localisation.factor = factor(data$hsa_anevrysme_localisation,levels=c("1","2","3","4","5","6","7","8"))
data$hsa_anevrysme_cote.factor = factor(data$hsa_anevrysme_cote,levels=c("1","2"))
data$hsa_anevrysme_traitement.factor = factor(data$hsa_anevrysme_traitement,levels=c("1","0"))
data$hsa_anevrysme_traitement_type.factor = factor(data$hsa_anevrysme_traitement_type,levels=c("1","2"))
data$hsa_anevrysme_complication.factor = factor(data$hsa_anevrysme_complication,levels=c("1","0"))
data$hsa_anevrysme_compli_type___1.factor = factor(data$hsa_anevrysme_compli_type___1,levels=c("0","1"))
data$hsa_anevrysme_compli_type___2.factor = factor(data$hsa_anevrysme_compli_type___2,levels=c("0","1"))
data$hsa_anevrysme_compli_type___3.factor = factor(data$hsa_anevrysme_compli_type___3,levels=c("0","1"))
data$hsa_anevrysme_compli_type___4.factor = factor(data$hsa_anevrysme_compli_type___4,levels=c("0","1"))
data$hsa_anevrysme_compli_type___5.factor = factor(data$hsa_anevrysme_compli_type___5,levels=c("0","1"))
data$hsa_anevrysme_nimodipine.factor = factor(data$hsa_anevrysme_nimodipine,levels=c("1","0"))
data$hsa_anevrysme_statine.factor = factor(data$hsa_anevrysme_statine,levels=c("1","0"))
data$hsa_dysfonction_cardiaque.factor = factor(data$hsa_dysfonction_cardiaque,levels=c("1","0"))
data$hsa_dysf_card_inotrope.factor = factor(data$hsa_dysf_card_inotrope,levels=c("1","0"))
data$hsa_vasospasme.factor = factor(data$hsa_vasospasme,levels=c("1","0"))
data$hsa_vasospasme_dg_type___1.factor = factor(data$hsa_vasospasme_dg_type___1,levels=c("0","1"))
data$hsa_vasospasme_dg_type___2.factor = factor(data$hsa_vasospasme_dg_type___2,levels=c("0","1"))
data$hsa_vasospasme_dg_type___3.factor = factor(data$hsa_vasospasme_dg_type___3,levels=c("0","1"))
data$hsa_vasospasme_dg_type___4.factor = factor(data$hsa_vasospasme_dg_type___4,levels=c("0","1"))
data$hsa_vasospasme_traitement.factor = factor(data$hsa_vasospasme_traitement,levels=c("1","0"))
data$hsa_vasospasme_traitement_type___1.factor = factor(data$hsa_vasospasme_traitement_type___1,levels=c("0","1"))
data$hsa_vasospasme_traitement_type___2.factor = factor(data$hsa_vasospasme_traitement_type___2,levels=c("0","1"))
data$hsa_vasospasme_traitement_type___3.factor = factor(data$hsa_vasospasme_traitement_type___3,levels=c("0","1"))
data$hsa_vasospasme_traitement_type___4.factor = factor(data$hsa_vasospasme_traitement_type___4,levels=c("0","1"))
data$hsa_vasospasme_traitement_type___5.factor = factor(data$hsa_vasospasme_traitement_type___5,levels=c("0","1"))
data$hsa_vasospasme_traitement_type___6.factor = factor(data$hsa_vasospasme_traitement_type___6,levels=c("0","1"))
data$hsa_vasospasme_territoire___1.factor = factor(data$hsa_vasospasme_territoire___1,levels=c("0","1"))
data$hsa_vasospasme_territoire___2.factor = factor(data$hsa_vasospasme_territoire___2,levels=c("0","1"))
data$hsa_vasospasme_territoire___3.factor = factor(data$hsa_vasospasme_territoire___3,levels=c("0","1"))
data$hsa_vasospasme_lateralite___1.factor = factor(data$hsa_vasospasme_lateralite___1,levels=c("0","1"))
data$hsa_vasospasme_lateralite___2.factor = factor(data$hsa_vasospasme_lateralite___2,levels=c("0","1"))
data$hemorragie_meningee_complete.factor = factor(data$hemorragie_meningee_complete,levels=c("0","1","2"))
data$hip_cause.factor = factor(data$hip_cause,levels=c("1","2","3","4","5","6","7"))
data$hiv.factor = factor(data$hiv,levels=c("1","0"))
data$hip_localisation___1.factor = factor(data$hip_localisation___1,levels=c("0","1"))
data$hip_localisation___2.factor = factor(data$hip_localisation___2,levels=c("0","1"))
data$hip_localisation___3.factor = factor(data$hip_localisation___3,levels=c("0","1"))
data$hip_localisation___4.factor = factor(data$hip_localisation___4,levels=c("0","1"))
data$hip_taille_30ml.factor = factor(data$hip_taille_30ml,levels=c("1","0"))
data$accident_vasculaire_cerebral_hemorragique_complete.factor = factor(data$accident_vasculaire_cerebral_hemorragique_complete,levels=c("0","1","2"))
data$aic_localisation___0.factor = factor(data$aic_localisation___0,levels=c("0","1"))
data$aic_localisation___1.factor = factor(data$aic_localisation___1,levels=c("0","1"))
data$aic_localisation___2.factor = factor(data$aic_localisation___2,levels=c("0","1"))
data$aic_localisation___3.factor = factor(data$aic_localisation___3,levels=c("0","1"))
data$aic_localisation___4.factor = factor(data$aic_localisation___4,levels=c("0","1"))
data$aic_localisation___5.factor = factor(data$aic_localisation___5,levels=c("0","1"))
data$aic_localisation___6.factor = factor(data$aic_localisation___6,levels=c("0","1"))
data$aic_localisation___7.factor = factor(data$aic_localisation___7,levels=c("0","1"))
data$aic_lateralite___1.factor = factor(data$aic_lateralite___1,levels=c("0","1"))
data$aic_lateralite___2.factor = factor(data$aic_lateralite___2,levels=c("0","1"))
data$aic_cause.factor = factor(data$aic_cause,levels=c("1","2","3","4","5"))
data$aic_thrombolyse.factor = factor(data$aic_thrombolyse,levels=c("1","0"))
data$aic_thrombectomie.factor = factor(data$aic_thrombectomie,levels=c("1","0"))
data$thrombectomie_anesthesie.factor = factor(data$thrombectomie_anesthesie,levels=c("1","2"))
data$aic_thrombectomie_revasc.factor = factor(data$aic_thrombectomie_revasc,levels=c("1","0"))
data$aic_thrombectomie_tici.factor = factor(data$aic_thrombectomie_tici,levels=c("1","2","3","4"))
data$aic_stent.factor = factor(data$aic_stent,levels=c("1","0"))
data$aic_stent_localisation___1.factor = factor(data$aic_stent_localisation___1,levels=c("0","1"))
data$aic_stent_localisation___2.factor = factor(data$aic_stent_localisation___2,levels=c("0","1"))
data$aic_stent_localisation___3.factor = factor(data$aic_stent_localisation___3,levels=c("0","1"))
data$aic_stent_localisation___4.factor = factor(data$aic_stent_localisation___4,levels=c("0","1"))
data$aic_stent_localisation___5.factor = factor(data$aic_stent_localisation___5,levels=c("0","1"))
data$aic_nri_complication.factor = factor(data$aic_nri_complication,levels=c("1","0"))
data$aic_nri_complication_type___1.factor = factor(data$aic_nri_complication_type___1,levels=c("0","1"))
data$aic_nri_complication_type___2.factor = factor(data$aic_nri_complication_type___2,levels=c("0","1"))
data$aic_nri_complication_type___3.factor = factor(data$aic_nri_complication_type___3,levels=c("0","1"))
data$aic_nri_complication_type___4.factor = factor(data$aic_nri_complication_type___4,levels=c("0","1"))
data$aic_nri_complication_type___5.factor = factor(data$aic_nri_complication_type___5,levels=c("0","1"))
data$aic_nri_complication_type___6.factor = factor(data$aic_nri_complication_type___6,levels=c("0","1"))
data$aic_nri_complication_type___7.factor = factor(data$aic_nri_complication_type___7,levels=c("0","1"))
data$aic_complication.factor = factor(data$aic_complication,levels=c("1","0"))
data$aic_complication_type___1.factor = factor(data$aic_complication_type___1,levels=c("0","1"))
data$aic_complication_type___2.factor = factor(data$aic_complication_type___2,levels=c("0","1"))
data$accident_vasculaire_ischemique_complete.factor = factor(data$accident_vasculaire_ischemique_complete,levels=c("0","1","2"))
data$inm_localisation___1.factor = factor(data$inm_localisation___1,levels=c("0","1"))
data$inm_localisation___2.factor = factor(data$inm_localisation___2,levels=c("0","1"))
data$inm_localisation___3.factor = factor(data$inm_localisation___3,levels=c("0","1"))
data$inm_localisation___4.factor = factor(data$inm_localisation___4,levels=c("0","1"))
data$inm_localisation___5.factor = factor(data$inm_localisation___5,levels=c("0","1"))
data$inm_communautaire.factor = factor(data$inm_communautaire,levels=c("1","0"))
data$inm_porte_entree.factor = factor(data$inm_porte_entree,levels=c("1","2","3","4","5"))
data$inm_prelevement.factor = factor(data$inm_prelevement,levels=c("1","0"))
data$inm_prelevement_type.factor = factor(data$inm_prelevement_type,levels=c("1","2","3","4"))
data$inm_prelevement_lactate.factor = factor(data$inm_prelevement_lactate,levels=c("1","0"))
data$inm_bacteriemie.factor = factor(data$inm_bacteriemie,levels=c("1","0"))
data$inm_microbe_1.factor = factor(data$inm_microbe_1,levels=c("1","2","3","4","5","6","7","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
data$inm_microbe_2.factor = factor(data$inm_microbe_2,levels=c("1","2","3","4","5","6","7","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
data$inm_microbe_3.factor = factor(data$inm_microbe_3,levels=c("1","2","3","4","5","6","7","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
data$inm_microbe_4.factor = factor(data$inm_microbe_4,levels=c("1","2","3","4","5","6","7","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
data$inm_microbe_5.factor = factor(data$inm_microbe_5,levels=c("1","2","3","4","5","6","7","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"))
data$inm_ttt_proba_mono.factor = factor(data$inm_ttt_proba_mono,levels=c("1","0"))
data$inm_ttt_proba_mol_1.factor = factor(data$inm_ttt_proba_mol_1,levels=c("1","2","3","4","5","6","7","8","9","10","11"))
data$inm_ttt_proba_mol_2.factor = factor(data$inm_ttt_proba_mol_2,levels=c("1","2","3","4","5","6","7","8","9","10","11"))
data$inm_ttt_docu_mono.factor = factor(data$inm_ttt_docu_mono,levels=c("1","0"))
data$inm_ttt_docu_mol_1.factor = factor(data$inm_ttt_docu_mol_1,levels=c("1","2","3","4","5","6","7","8","9","10","11"))
data$inm_ttt_docu_mol_2.factor = factor(data$inm_ttt_docu_mol_2,levels=c("1","2","3","4","5","6","7","8","9","10","11"))
data$inm_chirurgie.factor = factor(data$inm_chirurgie,levels=c("1","0"))
data$inm_evolution.factor = factor(data$inm_evolution,levels=c("1","2","3","4"))
data$infection_neuromeningee_complete.factor = factor(data$infection_neuromeningee_complete,levels=c("0","1","2"))
data$tumeur_atcd.factor = factor(data$tumeur_atcd,levels=c("1","0"))
data$tumeur_localisation___1.factor = factor(data$tumeur_localisation___1,levels=c("0","1"))
data$tumeur_localisation___2.factor = factor(data$tumeur_localisation___2,levels=c("0","1"))
data$tumeur_localisation___3.factor = factor(data$tumeur_localisation___3,levels=c("0","1"))
data$tumeur_localisation___4.factor = factor(data$tumeur_localisation___4,levels=c("0","1"))
data$tumeur_localisation___5.factor = factor(data$tumeur_localisation___5,levels=c("0","1"))
data$tumeur_localisation___6.factor = factor(data$tumeur_localisation___6,levels=c("0","1"))
data$tumeur_localisation___7.factor = factor(data$tumeur_localisation___7,levels=c("0","1"))
data$tumeur_cause_degr___1.factor = factor(data$tumeur_cause_degr___1,levels=c("0","1"))
data$tumeur_cause_degr___2.factor = factor(data$tumeur_cause_degr___2,levels=c("0","1"))
data$tumeur_cause_degr___3.factor = factor(data$tumeur_cause_degr___3,levels=c("0","1"))
data$tumeur_cause_degr___4.factor = factor(data$tumeur_cause_degr___4,levels=c("0","1"))
data$tumeur_cause_degr___5.factor = factor(data$tumeur_cause_degr___5,levels=c("0","1"))
data$tumeur_imagerie_diag___1.factor = factor(data$tumeur_imagerie_diag___1,levels=c("0","1"))
data$tumeur_imagerie_diag___2.factor = factor(data$tumeur_imagerie_diag___2,levels=c("0","1"))
data$tumeur_imagerie_diag___3.factor = factor(data$tumeur_imagerie_diag___3,levels=c("0","1"))
data$tumeur_imagerie_diag_inj.factor = factor(data$tumeur_imagerie_diag_inj,levels=c("1","0"))
data$tumeur_confirm_diag.factor = factor(data$tumeur_confirm_diag,levels=c("1","0"))
data$tumeur_confirm_diag_proc___1.factor = factor(data$tumeur_confirm_diag_proc___1,levels=c("0","1"))
data$tumeur_confirm_diag_proc___2.factor = factor(data$tumeur_confirm_diag_proc___2,levels=c("0","1"))
data$tumeur_confirm_diag_proc___3.factor = factor(data$tumeur_confirm_diag_proc___3,levels=c("0","1"))
data$tumeur_anapath.factor = factor(data$tumeur_anapath,levels=c("1","0"))
data$tumeur_anapath_benin.factor = factor(data$tumeur_anapath_benin,levels=c("1","0"))
data$tumeur_anapath_type___1.factor = factor(data$tumeur_anapath_type___1,levels=c("0","1"))
data$tumeur_anapath_type___2.factor = factor(data$tumeur_anapath_type___2,levels=c("0","1"))
data$tumeur_anapath_type___3.factor = factor(data$tumeur_anapath_type___3,levels=c("0","1"))
data$tumeur_anapath_type___4.factor = factor(data$tumeur_anapath_type___4,levels=c("0","1"))
data$tumeur_anapath_type___5.factor = factor(data$tumeur_anapath_type___5,levels=c("0","1"))
data$tumeur_anapath_type___6.factor = factor(data$tumeur_anapath_type___6,levels=c("0","1"))
data$tumeur_anapath_type___7.factor = factor(data$tumeur_anapath_type___7,levels=c("0","1"))
data$tumeur_ttt_urgent.factor = factor(data$tumeur_ttt_urgent,levels=c("1","0"))
data$tumeur_ttt_type___1.factor = factor(data$tumeur_ttt_type___1,levels=c("0","1"))
data$tumeur_ttt_type___2.factor = factor(data$tumeur_ttt_type___2,levels=c("0","1"))
data$tumeur_ttt_type___3.factor = factor(data$tumeur_ttt_type___3,levels=c("0","1"))
data$tumeur_ttt_type___4.factor = factor(data$tumeur_ttt_type___4,levels=c("0","1"))
data$tumeur_ttt_type___5.factor = factor(data$tumeur_ttt_type___5,levels=c("0","1"))
data$tumeur_ttt_type___6.factor = factor(data$tumeur_ttt_type___6,levels=c("0","1"))
data$tumeur_ttt_exer.factor = factor(data$tumeur_ttt_exer,levels=c("1","0"))
data$tumeur_ttt_exer_compl.factor = factor(data$tumeur_ttt_exer_compl,levels=c("1","0"))
data$tumeur_ttt_adj.factor = factor(data$tumeur_ttt_adj,levels=c("1","0"))
data$tumeur_adj_ttt_adj_type___1.factor = factor(data$tumeur_adj_ttt_adj_type___1,levels=c("0","1"))
data$tumeur_adj_ttt_adj_type___2.factor = factor(data$tumeur_adj_ttt_adj_type___2,levels=c("0","1"))
data$tumeur_adj_ttt_adj_type___3.factor = factor(data$tumeur_adj_ttt_adj_type___3,levels=c("0","1"))
data$tumeur_adj_ttt_adj_type___4.factor = factor(data$tumeur_adj_ttt_adj_type___4,levels=c("0","1"))
data$tumeur_complete.factor = factor(data$tumeur_complete,levels=c("0","1","2"))
data$capteur_pic.factor = factor(data$capteur_pic,levels=c("1","0"))
data$htic.factor = factor(data$htic,levels=c("1","0"))
data$htic_traitement___1.factor = factor(data$htic_traitement___1,levels=c("0","1"))
data$htic_traitement___2.factor = factor(data$htic_traitement___2,levels=c("0","1"))
data$htic_traitement___3.factor = factor(data$htic_traitement___3,levels=c("0","1"))
data$htic_traitement___4.factor = factor(data$htic_traitement___4,levels=c("0","1"))
data$htic_traitement___5.factor = factor(data$htic_traitement___5,levels=c("0","1"))
data$htic_traitement___11.factor = factor(data$htic_traitement___11,levels=c("0","1"))
data$htic_traitement___6.factor = factor(data$htic_traitement___6,levels=c("0","1"))
data$htic_traitement___7.factor = factor(data$htic_traitement___7,levels=c("0","1"))
data$htic_traitement___8.factor = factor(data$htic_traitement___8,levels=c("0","1"))
data$htic_traitement___9.factor = factor(data$htic_traitement___9,levels=c("0","1"))
data$htic_traitement___10.factor = factor(data$htic_traitement___10,levels=c("0","1"))
data$htic_traitement_hypnotique___1.factor = factor(data$htic_traitement_hypnotique___1,levels=c("0","1"))
data$htic_traitement_hypnotique___2.factor = factor(data$htic_traitement_hypnotique___2,levels=c("0","1"))
data$htic_traitement_hypnotique___3.factor = factor(data$htic_traitement_hypnotique___3,levels=c("0","1"))
data$htic_traitement_hypnotique___4.factor = factor(data$htic_traitement_hypnotique___4,levels=c("0","1"))
data$htic_traitement_curare___1.factor = factor(data$htic_traitement_curare___1,levels=c("0","1"))
data$htic_traitement_curare___2.factor = factor(data$htic_traitement_curare___2,levels=c("0","1"))
data$htic_traitement_curare___3.factor = factor(data$htic_traitement_curare___3,levels=c("0","1"))
data$htic_traitement_burst_type.factor = factor(data$htic_traitement_burst_type,levels=c("1","2"))
data$corticoides.factor = factor(data$corticoides,levels=c("1","0"))
data$monitorage_tcd.factor = factor(data$monitorage_tcd,levels=c("1","0"))
data$dtc_bf_rea.factor = factor(data$dtc_bf_rea,levels=c("1","0"))
data$capteur_ptio2.factor = factor(data$capteur_ptio2,levels=c("1","0"))
data$capteur_svjo2.factor = factor(data$capteur_svjo2,levels=c("1","0"))
data$microdialyse.factor = factor(data$microdialyse,levels=c("1","0"))
data$eeg_continu.factor = factor(data$eeg_continu,levels=c("1","0"))
data$neurochirurgie.factor = factor(data$neurochirurgie,levels=c("1","0"))
data$neurochirurgie_type___1.factor = factor(data$neurochirurgie_type___1,levels=c("0","1"))
data$neurochirurgie_type___2.factor = factor(data$neurochirurgie_type___2,levels=c("0","1"))
data$neurochirurgie_type___7.factor = factor(data$neurochirurgie_type___7,levels=c("0","1"))
data$neurochirurgie_type___3.factor = factor(data$neurochirurgie_type___3,levels=c("0","1"))
data$neurochirurgie_type___4.factor = factor(data$neurochirurgie_type___4,levels=c("0","1"))
data$neurochirurgie_type___6.factor = factor(data$neurochirurgie_type___6,levels=c("0","1"))
data$neurochirurgie_type___5.factor = factor(data$neurochirurgie_type___5,levels=c("0","1"))
data$epilepsie.factor = factor(data$epilepsie,levels=c("1","0"))
data$epilepsie_edme.factor = factor(data$epilepsie_edme,levels=c("1","0"))
data$epilepsie_edme_refrac.factor = factor(data$epilepsie_edme_refrac,levels=c("1","0"))
data$epilepsie_traitement___1.factor = factor(data$epilepsie_traitement___1,levels=c("0","1"))
data$epilepsie_traitement___2.factor = factor(data$epilepsie_traitement___2,levels=c("0","1"))
data$epilepsie_traitement___3.factor = factor(data$epilepsie_traitement___3,levels=c("0","1"))
data$epilepsie_traitement___4.factor = factor(data$epilepsie_traitement___4,levels=c("0","1"))
data$epilepsie_traitement___5.factor = factor(data$epilepsie_traitement___5,levels=c("0","1"))
data$epilepsie_traitement___6.factor = factor(data$epilepsie_traitement___6,levels=c("0","1"))
data$epilepsie_traitement___7.factor = factor(data$epilepsie_traitement___7,levels=c("0","1"))
data$ventilation_rea.factor = factor(data$ventilation_rea,levels=c("1","0"))
data$infection_rea.factor = factor(data$infection_rea,levels=c("1","0"))
data$infection_rea_precoce.factor = factor(data$infection_rea_precoce,levels=c("1","0"))
data$infection_rea_precoce_inh.factor = factor(data$infection_rea_precoce_inh,levels=c("1","0"))
data$infection_rea_tardive.factor = factor(data$infection_rea_tardive,levels=c("1","0"))
data$infection_rea_tard_type___1.factor = factor(data$infection_rea_tard_type___1,levels=c("0","1"))
data$infection_rea_tard_type___2.factor = factor(data$infection_rea_tard_type___2,levels=c("0","1"))
data$infection_rea_tard_type___3.factor = factor(data$infection_rea_tard_type___3,levels=c("0","1"))
data$infection_rea_tard_type___4.factor = factor(data$infection_rea_tard_type___4,levels=c("0","1"))
data$infection_rea_tard_type___5.factor = factor(data$infection_rea_tard_type___5,levels=c("0","1"))
data$infection_rea_choc.factor = factor(data$infection_rea_choc,levels=c("1","0"))
data$sedation_rea.factor = factor(data$sedation_rea,levels=c("1","0"))
data$sedation_rea_type___1.factor = factor(data$sedation_rea_type___1,levels=c("0","1"))
data$sedation_rea_type___2.factor = factor(data$sedation_rea_type___2,levels=c("0","1"))
data$sedation_rea_type___3.factor = factor(data$sedation_rea_type___3,levels=c("0","1"))
data$sedation_rea_type___4.factor = factor(data$sedation_rea_type___4,levels=c("0","1"))
data$sedation_rea_type___5.factor = factor(data$sedation_rea_type___5,levels=c("0","1"))
data$trachetomie.factor = factor(data$trachetomie,levels=c("1","0"))
data$tbes_deglutition.factor = factor(data$tbes_deglutition,levels=c("1","0"))
data$gpe.factor = factor(data$gpe,levels=c("1","0"))
data$lata.factor = factor(data$lata,levels=c("1","0"))
data$ata.factor = factor(data$ata,levels=c("1","0"))
data$deces_rea.factor = factor(data$deces_rea,levels=c("1","0"))
data$edme.factor = factor(data$edme,levels=c("1","0"))
data$pmo.factor = factor(data$pmo,levels=c("1","0"))
data$pmo_maastricht.factor = factor(data$pmo_maastricht,levels=c("1","2"))
data$edme_confirmation.factor = factor(data$edme_confirmation,levels=c("1","2","3"))
data$prise_en_charge_en_reanimation_complete.factor = factor(data$prise_en_charge_en_reanimation_complete,levels=c("0","1","2"))
data$sortie_rea.factor = factor(data$sortie_rea,levels=c("1","2","3","4","5","6","7","10","11","12","8","9"))
data$ouverture_yeux.factor = factor(data$ouverture_yeux,levels=c("1","0"))
data$etat_conscience.factor = factor(data$etat_conscience,levels=c("1","2","3"))
data$crsr.factor = factor(data$crsr,levels=c("1","0"))
data$eval_distance.factor = factor(data$eval_distance,levels=c("1","0"))
data$evaluation_distance_deces.factor = factor(data$evaluation_distance_deces,levels=c("1","0"))
data$evaluation_distance_rad.factor = factor(data$evaluation_distance_rad,levels=c("1","0"))
data$evaluation_distance_pec_spe.factor = factor(data$evaluation_distance_pec_spe,levels=c("1","0"))
data$eval_distance_score___1.factor = factor(data$eval_distance_score___1,levels=c("0","1"))
data$eval_distance_score___5.factor = factor(data$eval_distance_score___5,levels=c("0","1"))
data$eval_distance_score___2.factor = factor(data$eval_distance_score___2,levels=c("0","1"))
data$eval_distance_score___3.factor = factor(data$eval_distance_score___3,levels=c("0","1"))
data$eval_distance_score___6.factor = factor(data$eval_distance_score___6,levels=c("0","1"))
data$eval_distance_score___4.factor = factor(data$eval_distance_score___4,levels=c("0","1"))
data$donnees_a_la_suite_de_la_reanimation_complete.factor = factor(data$donnees_a_la_suite_de_la_reanimation_complete,levels=c("0","1","2"))

levels(data$criteres_inclusion___1.factor)=c("Unchecked","Checked")
levels(data$criteres_inclusion___2.factor)=c("Unchecked","Checked")
levels(data$criteres_inclusion___3.factor)=c("Unchecked","Checked")
levels(data$criteres_inclusion___4.factor)=c("Unchecked","Checked")
levels(data$centre_inclusion.factor)=c("Bicêtre","Lariboisière","Beaujon","Pitié-Salpétrière","Henri Mondor")
levels(data$traumabase.factor)=c("Oui","Non")
levels(data$sex.factor)=c("M","F","Inconnu")
levels(data$femme_peripartum.factor)=c("Oui","Non")
levels(data$more_than_2_years_old.factor)=c("Oui","Non")
levels(data$ieyn.factor)=c("Oui","Non")
levels(data$donnes_dmographiques_aphp_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$hta.factor)=c("Oui","Non","Non disponible")
levels(data$hta_traitement.factor)=c("Oui","Non")
levels(data$diabete.factor)=c("Type 1","Type 2 non insulino-requerant","Type 2 insulino-requerant","Non","Non disponible")
levels(data$tabac.factor)=c("Actif","Sevre","Non")
levels(data$intox_chronique___1.factor)=c("Unchecked","Checked")
levels(data$intox_chronique___2.factor)=c("Unchecked","Checked")
levels(data$intox_chronique___3.factor)=c("Unchecked","Checked")
levels(data$intox_chronique___4.factor)=c("Unchecked","Checked")
levels(data$intox_chronique___5.factor)=c("Unchecked","Checked")
levels(data$intox_aigue___1.factor)=c("Unchecked","Checked")
levels(data$intox_aigue___2.factor)=c("Unchecked","Checked")
levels(data$intox_aigue___3.factor)=c("Unchecked","Checked")
levels(data$intox_aigue___4.factor)=c("Unchecked","Checked")
levels(data$intox_aigue___5.factor)=c("Unchecked","Checked")
levels(data$aap.factor)=c("Oui","Non")
levels(data$aap_type___1.factor)=c("Unchecked","Checked")
levels(data$aap_type___2.factor)=c("Unchecked","Checked")
levels(data$aap_type___3.factor)=c("Unchecked","Checked")
levels(data$aap_type___4.factor)=c("Unchecked","Checked")
levels(data$aap_type___5.factor)=c("Unchecked","Checked")
levels(data$aap_type___6.factor)=c("Unchecked","Checked")
levels(data$aap_type___7.factor)=c("Unchecked","Checked")
levels(data$lateralite.factor)=c("Gaucher","Droitier","Ambidextre","Non disponible")
levels(data$anticoagulant.factor)=c("Oui","Non")
levels(data$anticoagulant_type___1.factor)=c("Unchecked","Checked")
levels(data$anticoagulant_type___2.factor)=c("Unchecked","Checked")
levels(data$anticoagulant_type___3.factor)=c("Unchecked","Checked")
levels(data$anticoagulant_type___4.factor)=c("Unchecked","Checked")
levels(data$francophone.factor)=c("Oui","Non")
levels(data$isolement.factor)=c("Oui","Non")
levels(data$activite.factor)=c("Etudiant","En activite","Chomage","Invalidite","Retraite","Non disponible","Sans domicile fixe","Incarcération","Soccupe des enfants")
levels(data$antecedents_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$pas_inf_90.factor)=c("Oui","Non")
levels(data$spo2_inf_90.factor)=c("Oui","Non")
levels(data$gcs_detaille.factor)=c("Oui","Non")
levels(data$gcs_yeux_initial.factor)=c("1","2","3","4")
levels(data$gcs_verbal_initial.factor)=c("1","2","3","4","5")
levels(data$gcs_moteur_initial.factor)=c("1","2","3","4","5","6")
levels(data$convulsions_initial.factor)=c("Oui","Non")
levels(data$pupille_gauche_admission_v2.factor)=c("Myosis","Intermédiaire","Mydriase")
levels(data$pupille_droite_admission_v2.factor)=c("Myosis","Intermédiaire","Mydriase")
levels(data$dtc_initial_fait.factor)=c("Oui","Non")
levels(data$bf_dtc_initial.factor)=c("Oui","Non")
levels(data$bf_dtc_initial_cote___1.factor)=c("Unchecked","Checked")
levels(data$bf_dtc_initial_cote___2.factor)=c("Unchecked","Checked")
levels(data$dtc_g_initial.factor)=c("Oui","Non")
levels(data$dtc_d_initial.factor)=c("Oui","Non")
levels(data$imagerie_prerea.factor)=c("Oui","Non")
levels(data$imagerie_prerea_type___1.factor)=c("Unchecked","Checked")
levels(data$imagerie_prerea_type___2.factor)=c("Unchecked","Checked")
levels(data$imagerie_prerea_injection.factor)=c("Oui","Non")
levels(data$malformation_vasculaire.factor)=c("Oui","Non")
levels(data$malformation_vasculaire_type___1.factor)=c("Unchecked","Checked")
levels(data$malformation_vasculaire_type___2.factor)=c("Unchecked","Checked")
levels(data$malformation_vasculaire_type___3.factor)=c("Unchecked","Checked")
levels(data$osmo_prerea.factor)=c("Oui","Non")
levels(data$osmo_prerea_indication___1.factor)=c("Unchecked","Checked")
levels(data$osmo_prerea_indication___2.factor)=c("Unchecked","Checked")
levels(data$osmo_prerea_indication___3.factor)=c("Unchecked","Checked")
levels(data$osmo_prerea_type___1.factor)=c("Unchecked","Checked")
levels(data$osmo_prerea_type___2.factor)=c("Unchecked","Checked")
levels(data$osmo_prerea_efficace.factor)=c("Oui","Non")
levels(data$intubation_prerea.factor)=c("Oui","Non")
levels(data$etco2_dispo.factor)=c("Oui","Non")
levels(data$norad_prerea.factor)=c("Oui","Non")
levels(data$norad_prerea_indication.factor)=c("Etat de choc","Perfusion cerebrale")
levels(data$acr_prehosp.factor)=c("Oui","Non")
levels(data$prise_en_charge_avant_la_reanimation_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$mode_admission.factor)=c("Urgence interne","Transfert primaire","Transfert secondaire")
levels(data$admission_ggnc_pdses.factor)=c("GGNC","PDSES","Aucun")
levels(data$sedation_admission.factor)=c("Oui","Non")
levels(data$convulsions_admission.factor)=c("Oui","Non")
levels(data$gcs_yeux_admission.factor)=c("1","2","3","4")
levels(data$gcs_verbal_admission.factor)=c("1","2","3","4","5")
levels(data$gcs_moteur_admission.factor)=c("1","2","3","4","5","6")
levels(data$gcs_yeux_pire_24_heures.factor)=c("1","2","3","4")
levels(data$gcs_verbal_pire_24_heures.factor)=c("1","2","3","4","5")
levels(data$gcs_moteur_pire_24_heures.factor)=c("1","2","3","4","5","6")
levels(data$pupille_gauche_admission.factor)=c("Myosis","Intermediaire","Mydriase")
levels(data$pupille_droite_admission.factor)=c("Myosis","Intermediaire","Mydriase")
levels(data$react_pupille_g.factor)=c("Oui","Non")
levels(data$react_pupille_d.factor)=c("Oui","Non")
levels(data$dtc_admission.factor)=c("Oui","Non")
levels(data$dtc_g_admission.factor)=c("Oui","Non")
levels(data$dtc_d_admission.factor)=c("Oui","Non")
levels(data$imagerie_rea_24_heures.factor)=c("Oui","Non")
levels(data$imagerie_rea_type___1.factor)=c("Unchecked","Checked")
levels(data$imagerie_rea_type___2.factor)=c("Unchecked","Checked")
levels(data$imagerie_rea_injection.factor)=c("Oui","Non")
levels(data$osmo_rea.factor)=c("Oui","Non")
levels(data$osmo_rea_indication.factor)=c("Anomalie pupillaire","Doppler pathologiques")
levels(data$osmo_rea_type.factor)=c("Mannitol","Serum sale hypertonique")
levels(data$osmo_rea_efficace.factor)=c("Oui","Non")
levels(data$intubation_rea.factor)=c("Oui","Non")
levels(data$norad_rea.factor)=c("Oui","Non")
levels(data$norad_rea_indication.factor)=c("Etat de choc","Perfusion cerebrale")
levels(data$diagnostic.factor)=c("Traumatisme cranien","Hemorragie meningee","Accident vasculaire cerebral hemorragique","Accident vasculaire cerebral ischemique","Infection neuro-meningee","Tumeur")
levels(data$prise_en_charge_initiale_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$mecanisme_trauma.factor)=c("Accident de la voie publique","Chute","Rixe","Inconnue","Autre")
levels(data$trauma_chute_6m.factor)=c("Oui","Non")
levels(data$trauma_penetrant.factor)=c("Oui","Non")
levels(data$trauma_lesions_scan___1.factor)=c("Unchecked","Checked")
levels(data$trauma_lesions_scan___2.factor)=c("Unchecked","Checked")
levels(data$trauma_lesions_scan___3.factor)=c("Unchecked","Checked")
levels(data$trauma_lesions_scan___4.factor)=c("Unchecked","Checked")
levels(data$trauma_lesions_scan___5.factor)=c("Unchecked","Checked")
levels(data$trauma_lesions_scan___6.factor)=c("Unchecked","Checked")
levels(data$trauma_lesions_scan___7.factor)=c("Unchecked","Checked")
levels(data$trauma_vasospasme.factor)=c("Oui","Non")
levels(data$trauma_lesions_associees.factor)=c("Oui","Non")
levels(data$trauma_hemorragie_intervention.factor)=c("Oui","Non")
levels(data$trauma_type_hemostase___1.factor)=c("Unchecked","Checked")
levels(data$trauma_type_hemostase___2.factor)=c("Unchecked","Checked")
levels(data$trauma_transfusion_2_cgr.factor)=c("Oui","Non")
levels(data$trauma_medullaire.factor)=c("Oui","Non")
levels(data$traumatisme_cranien_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$hsa_perte_conscience.factor)=c("Oui","Non")
levels(data$hsa_anevrysme.factor)=c("Oui","Non")
levels(data$hsa_anevrysme_mav.factor)=c("Oui","Non")
levels(data$hsa_anevrysme_localisation.factor)=c("Artere cerebrale moyenne","Artere cerebrale anterieure","Artere communicante anterieure","Artere communicante posterieure","Artere cerebrale posterieure","Tronc basilaire","Artère vertebrale","Terminaison carotidienne")
levels(data$hsa_anevrysme_cote.factor)=c("Gauche","Droite")
levels(data$hsa_anevrysme_traitement.factor)=c("Oui","Non")
levels(data$hsa_anevrysme_traitement_type.factor)=c("Radiologie interventionnelle","Chirurgie")
levels(data$hsa_anevrysme_complication.factor)=c("Oui","Non")
levels(data$hsa_anevrysme_compli_type___1.factor)=c("Unchecked","Checked")
levels(data$hsa_anevrysme_compli_type___2.factor)=c("Unchecked","Checked")
levels(data$hsa_anevrysme_compli_type___3.factor)=c("Unchecked","Checked")
levels(data$hsa_anevrysme_compli_type___4.factor)=c("Unchecked","Checked")
levels(data$hsa_anevrysme_compli_type___5.factor)=c("Unchecked","Checked")
levels(data$hsa_anevrysme_nimodipine.factor)=c("Oui","Non")
levels(data$hsa_anevrysme_statine.factor)=c("Oui","Non")
levels(data$hsa_dysfonction_cardiaque.factor)=c("Oui","Non")
levels(data$hsa_dysf_card_inotrope.factor)=c("Oui","Non")
levels(data$hsa_vasospasme.factor)=c("Oui","Non")
levels(data$hsa_vasospasme_dg_type___1.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_dg_type___2.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_dg_type___3.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_dg_type___4.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_traitement.factor)=c("Oui","Non")
levels(data$hsa_vasospasme_traitement_type___1.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_traitement_type___2.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_traitement_type___3.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_traitement_type___4.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_traitement_type___5.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_traitement_type___6.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_territoire___1.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_territoire___2.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_territoire___3.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_lateralite___1.factor)=c("Unchecked","Checked")
levels(data$hsa_vasospasme_lateralite___2.factor)=c("Unchecked","Checked")
levels(data$hemorragie_meningee_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$hip_cause.factor)=c("Arteriosclerose (HTA)","Malformation arterio-veineuse","Tumeur","Inconnu","Autre","Thrombophlébite cérébrale","Angiopathie amyloïde")
levels(data$hiv.factor)=c("Oui","Non")
levels(data$hip_localisation___1.factor)=c("Unchecked","Checked")
levels(data$hip_localisation___2.factor)=c("Unchecked","Checked")
levels(data$hip_localisation___3.factor)=c("Unchecked","Checked")
levels(data$hip_localisation___4.factor)=c("Unchecked","Checked")
levels(data$hip_taille_30ml.factor)=c("Oui","Non")
levels(data$accident_vasculaire_cerebral_hemorragique_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$aic_localisation___0.factor)=c("Unchecked","Checked")
levels(data$aic_localisation___1.factor)=c("Unchecked","Checked")
levels(data$aic_localisation___2.factor)=c("Unchecked","Checked")
levels(data$aic_localisation___3.factor)=c("Unchecked","Checked")
levels(data$aic_localisation___4.factor)=c("Unchecked","Checked")
levels(data$aic_localisation___5.factor)=c("Unchecked","Checked")
levels(data$aic_localisation___6.factor)=c("Unchecked","Checked")
levels(data$aic_localisation___7.factor)=c("Unchecked","Checked")
levels(data$aic_lateralite___1.factor)=c("Unchecked","Checked")
levels(data$aic_lateralite___2.factor)=c("Unchecked","Checked")
levels(data$aic_cause.factor)=c("Athérome","Cardiopathie emboligène","Lacune","Autres causes","Inconnue")
levels(data$aic_thrombolyse.factor)=c("Oui","Non")
levels(data$aic_thrombectomie.factor)=c("Oui","Non")
levels(data$thrombectomie_anesthesie.factor)=c("Anesthesie generale","Sedation")
levels(data$aic_thrombectomie_revasc.factor)=c("Oui","Non")
levels(data$aic_thrombectomie_tici.factor)=c("0","1","2","3")
levels(data$aic_stent.factor)=c("Oui","Non")
levels(data$aic_stent_localisation___1.factor)=c("Unchecked","Checked")
levels(data$aic_stent_localisation___2.factor)=c("Unchecked","Checked")
levels(data$aic_stent_localisation___3.factor)=c("Unchecked","Checked")
levels(data$aic_stent_localisation___4.factor)=c("Unchecked","Checked")
levels(data$aic_stent_localisation___5.factor)=c("Unchecked","Checked")
levels(data$aic_nri_complication.factor)=c("Oui","Non")
levels(data$aic_nri_complication_type___1.factor)=c("Unchecked","Checked")
levels(data$aic_nri_complication_type___2.factor)=c("Unchecked","Checked")
levels(data$aic_nri_complication_type___3.factor)=c("Unchecked","Checked")
levels(data$aic_nri_complication_type___4.factor)=c("Unchecked","Checked")
levels(data$aic_nri_complication_type___5.factor)=c("Unchecked","Checked")
levels(data$aic_nri_complication_type___6.factor)=c("Unchecked","Checked")
levels(data$aic_nri_complication_type___7.factor)=c("Unchecked","Checked")
levels(data$aic_complication.factor)=c("Oui","Non")
levels(data$aic_complication_type___1.factor)=c("Unchecked","Checked")
levels(data$aic_complication_type___2.factor)=c("Unchecked","Checked")
levels(data$accident_vasculaire_ischemique_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$inm_localisation___1.factor)=c("Unchecked","Checked")
levels(data$inm_localisation___2.factor)=c("Unchecked","Checked")
levels(data$inm_localisation___3.factor)=c("Unchecked","Checked")
levels(data$inm_localisation___4.factor)=c("Unchecked","Checked")
levels(data$inm_localisation___5.factor)=c("Unchecked","Checked")
levels(data$inm_communautaire.factor)=c("Oui","Non")
levels(data$inm_porte_entree.factor)=c("Infection ORL","Infection après DVE/DVP","Infection après neurochirurgie autre que DVE/DVP","Inconnue","Autre")
levels(data$inm_prelevement.factor)=c("Oui","Non")
levels(data$inm_prelevement_type.factor)=c("Prélèvement per-opératoire","Ponction lombaire","LCR par DVE","LCR par ponction boîtier DVP")
levels(data$inm_prelevement_lactate.factor)=c("Oui","Non")
levels(data$inm_bacteriemie.factor)=c("Oui","Non")
levels(data$inm_microbe_1.factor)=c("Méningocoque","Escherichia coli","Haemophilus","Klebsiella","Enterobacter","Acinetobacter","Pseudomonas","Autre BGN","SAMS","SARM","Staphylocoque coagulase négative","Pneumocoque","Autre streptocoque","Propionibacterium acnes","Autre CGP","Germe anaérobie","Candida","Autre champignon","HSV","Autre virus","Toxoplasmose","Autre parasite")
levels(data$inm_microbe_2.factor)=c("Méningocoque","Escherichia coli","Haemophilus","Klebsiella","Enterobacter","Acinetobacter","Pseudomonas","Autre BGN","SAMS","SARM","Staphylocoque coagulase négative","Pneumocoque","Autre streptocoque","Propionibacterium acnes","Autre CGP","Germe anaérobie","Candida","Autre champignon","HSV","Autre virus","Toxoplasmose","Autre parasite")
levels(data$inm_microbe_3.factor)=c("Méningocoque","Escherichia coli","Haemophilus","Klebsiella","Enterobacter","Acinetobacter","Pseudomonas","Autre BGN","SAMS","SARM","Staphylocoque coagulase négative","Pneumocoque","Autre streptocoque","Propionibacterium acnes","Autre CGP","Germe anaérobie","Candida","Autre champignon","HSV","Autre virus","Toxoplasmose","Autre parasite")
levels(data$inm_microbe_4.factor)=c("Méningocoque","Escherichia coli","Haemophilus","Klebsiella","Enterobacter","Acinetobacter","Pseudomonas","Autre BGN","SAMS","SARM","Staphylocoque coagulase négative","Pneumocoque","Autre streptocoque","Propionibacterium acnes","Autre CGP","Germe anaérobie","Candida","Autre champignon","HSV","Autre virus","Toxoplasmose","Autre parasite")
levels(data$inm_microbe_5.factor)=c("Méningocoque","Escherichia coli","Haemophilus","Klebsiella","Enterobacter","Acinetobacter","Pseudomonas","Autre BGN","SAMS","SARM","Staphylocoque coagulase négative","Pneumocoque","Autre streptocoque","Propionibacterium acnes","Autre CGP","Germe anaérobie","Candida","Autre champignon","HSV","Autre virus","Toxoplasmose","Autre parasite")
levels(data$inm_ttt_proba_mono.factor)=c("Oui","Non")
levels(data$inm_ttt_proba_mol_1.factor)=c("Pénicilille A","Pénicilline M","C3G","C4G","Carbapeneme","Vancomycine","Linezolide","Rifampicine","Fluoroquinolone","Aciclovir","Autre")
levels(data$inm_ttt_proba_mol_2.factor)=c("Pénicilille A","Pénicilline M","C3G","C4G","Carbapeneme","Vancomycine","Linezolide","Rifampicine","Fluoroquinolone","Aciclovir","Autre")
levels(data$inm_ttt_docu_mono.factor)=c("Oui","Non")
levels(data$inm_ttt_docu_mol_1.factor)=c("Pénicilille A","Pénicilline M","C3G","C4G","Carbapeneme","Vancomycine","Linezolide","Rifampicine","Fluoroquinolone","Aciclovir","Autre")
levels(data$inm_ttt_docu_mol_2.factor)=c("Pénicilille A","Pénicilline M","C3G","C4G","Carbapeneme","Vancomycine","Linezolide","Rifampicine","Fluoroquinolone","Aciclovir","Autre")
levels(data$inm_chirurgie.factor)=c("Oui","Non")
levels(data$inm_evolution.factor)=c("Guérison sans récidive","Récidive de linfection","Autre infection neuro-méningée","Décès")
levels(data$infection_neuromeningee_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$tumeur_atcd.factor)=c("Oui","Non")
levels(data$tumeur_localisation___1.factor)=c("Unchecked","Checked")
levels(data$tumeur_localisation___2.factor)=c("Unchecked","Checked")
levels(data$tumeur_localisation___3.factor)=c("Unchecked","Checked")
levels(data$tumeur_localisation___4.factor)=c("Unchecked","Checked")
levels(data$tumeur_localisation___5.factor)=c("Unchecked","Checked")
levels(data$tumeur_localisation___6.factor)=c("Unchecked","Checked")
levels(data$tumeur_localisation___7.factor)=c("Unchecked","Checked")
levels(data$tumeur_cause_degr___1.factor)=c("Unchecked","Checked")
levels(data$tumeur_cause_degr___2.factor)=c("Unchecked","Checked")
levels(data$tumeur_cause_degr___3.factor)=c("Unchecked","Checked")
levels(data$tumeur_cause_degr___4.factor)=c("Unchecked","Checked")
levels(data$tumeur_cause_degr___5.factor)=c("Unchecked","Checked")
levels(data$tumeur_imagerie_diag___1.factor)=c("Unchecked","Checked")
levels(data$tumeur_imagerie_diag___2.factor)=c("Unchecked","Checked")
levels(data$tumeur_imagerie_diag___3.factor)=c("Unchecked","Checked")
levels(data$tumeur_imagerie_diag_inj.factor)=c("Oui","Non")
levels(data$tumeur_confirm_diag.factor)=c("Oui","Non")
levels(data$tumeur_confirm_diag_proc___1.factor)=c("Unchecked","Checked")
levels(data$tumeur_confirm_diag_proc___2.factor)=c("Unchecked","Checked")
levels(data$tumeur_confirm_diag_proc___3.factor)=c("Unchecked","Checked")
levels(data$tumeur_anapath.factor)=c("Oui","Non")
levels(data$tumeur_anapath_benin.factor)=c("Oui","Non")
levels(data$tumeur_anapath_type___1.factor)=c("Unchecked","Checked")
levels(data$tumeur_anapath_type___2.factor)=c("Unchecked","Checked")
levels(data$tumeur_anapath_type___3.factor)=c("Unchecked","Checked")
levels(data$tumeur_anapath_type___4.factor)=c("Unchecked","Checked")
levels(data$tumeur_anapath_type___5.factor)=c("Unchecked","Checked")
levels(data$tumeur_anapath_type___6.factor)=c("Unchecked","Checked")
levels(data$tumeur_anapath_type___7.factor)=c("Unchecked","Checked")
levels(data$tumeur_ttt_urgent.factor)=c("Oui","Non")
levels(data$tumeur_ttt_type___1.factor)=c("Unchecked","Checked")
levels(data$tumeur_ttt_type___2.factor)=c("Unchecked","Checked")
levels(data$tumeur_ttt_type___3.factor)=c("Unchecked","Checked")
levels(data$tumeur_ttt_type___4.factor)=c("Unchecked","Checked")
levels(data$tumeur_ttt_type___5.factor)=c("Unchecked","Checked")
levels(data$tumeur_ttt_type___6.factor)=c("Unchecked","Checked")
levels(data$tumeur_ttt_exer.factor)=c("Oui","Non")
levels(data$tumeur_ttt_exer_compl.factor)=c("Oui","Non")
levels(data$tumeur_ttt_adj.factor)=c("Oui","Non")
levels(data$tumeur_adj_ttt_adj_type___1.factor)=c("Unchecked","Checked")
levels(data$tumeur_adj_ttt_adj_type___2.factor)=c("Unchecked","Checked")
levels(data$tumeur_adj_ttt_adj_type___3.factor)=c("Unchecked","Checked")
levels(data$tumeur_adj_ttt_adj_type___4.factor)=c("Unchecked","Checked")
levels(data$tumeur_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$capteur_pic.factor)=c("Oui","Non")
levels(data$htic.factor)=c("Oui","Non")
levels(data$htic_traitement___1.factor)=c("Unchecked","Checked")
levels(data$htic_traitement___2.factor)=c("Unchecked","Checked")
levels(data$htic_traitement___3.factor)=c("Unchecked","Checked")
levels(data$htic_traitement___4.factor)=c("Unchecked","Checked")
levels(data$htic_traitement___5.factor)=c("Unchecked","Checked")
levels(data$htic_traitement___11.factor)=c("Unchecked","Checked")
levels(data$htic_traitement___6.factor)=c("Unchecked","Checked")
levels(data$htic_traitement___7.factor)=c("Unchecked","Checked")
levels(data$htic_traitement___8.factor)=c("Unchecked","Checked")
levels(data$htic_traitement___9.factor)=c("Unchecked","Checked")
levels(data$htic_traitement___10.factor)=c("Unchecked","Checked")
levels(data$htic_traitement_hypnotique___1.factor)=c("Unchecked","Checked")
levels(data$htic_traitement_hypnotique___2.factor)=c("Unchecked","Checked")
levels(data$htic_traitement_hypnotique___3.factor)=c("Unchecked","Checked")
levels(data$htic_traitement_hypnotique___4.factor)=c("Unchecked","Checked")
levels(data$htic_traitement_curare___1.factor)=c("Unchecked","Checked")
levels(data$htic_traitement_curare___2.factor)=c("Unchecked","Checked")
levels(data$htic_traitement_curare___3.factor)=c("Unchecked","Checked")
levels(data$htic_traitement_burst_type.factor)=c("Propofol","Thiopental")
levels(data$corticoides.factor)=c("Oui","Non")
levels(data$monitorage_tcd.factor)=c("Oui","Non")
levels(data$dtc_bf_rea.factor)=c("Oui","Non")
levels(data$capteur_ptio2.factor)=c("Oui","Non")
levels(data$capteur_svjo2.factor)=c("Oui","Non")
levels(data$microdialyse.factor)=c("Oui","Non")
levels(data$eeg_continu.factor)=c("Oui","Non")
levels(data$neurochirurgie.factor)=c("Oui","Non")
levels(data$neurochirurgie_type___1.factor)=c("Unchecked","Checked")
levels(data$neurochirurgie_type___2.factor)=c("Unchecked","Checked")
levels(data$neurochirurgie_type___7.factor)=c("Unchecked","Checked")
levels(data$neurochirurgie_type___3.factor)=c("Unchecked","Checked")
levels(data$neurochirurgie_type___4.factor)=c("Unchecked","Checked")
levels(data$neurochirurgie_type___6.factor)=c("Unchecked","Checked")
levels(data$neurochirurgie_type___5.factor)=c("Unchecked","Checked")
levels(data$epilepsie.factor)=c("Oui","Non")
levels(data$epilepsie_edme.factor)=c("Oui","Non")
levels(data$epilepsie_edme_refrac.factor)=c("Oui","Non")
levels(data$epilepsie_traitement___1.factor)=c("Unchecked","Checked")
levels(data$epilepsie_traitement___2.factor)=c("Unchecked","Checked")
levels(data$epilepsie_traitement___3.factor)=c("Unchecked","Checked")
levels(data$epilepsie_traitement___4.factor)=c("Unchecked","Checked")
levels(data$epilepsie_traitement___5.factor)=c("Unchecked","Checked")
levels(data$epilepsie_traitement___6.factor)=c("Unchecked","Checked")
levels(data$epilepsie_traitement___7.factor)=c("Unchecked","Checked")
levels(data$ventilation_rea.factor)=c("Oui","Non")
levels(data$infection_rea.factor)=c("Oui","Non")
levels(data$infection_rea_precoce.factor)=c("Oui","Non")
levels(data$infection_rea_precoce_inh.factor)=c("Oui","Non")
levels(data$infection_rea_tardive.factor)=c("Oui","Non")
levels(data$infection_rea_tard_type___1.factor)=c("Unchecked","Checked")
levels(data$infection_rea_tard_type___2.factor)=c("Unchecked","Checked")
levels(data$infection_rea_tard_type___3.factor)=c("Unchecked","Checked")
levels(data$infection_rea_tard_type___4.factor)=c("Unchecked","Checked")
levels(data$infection_rea_tard_type___5.factor)=c("Unchecked","Checked")
levels(data$infection_rea_choc.factor)=c("Oui","Non")
levels(data$sedation_rea.factor)=c("Oui","Non")
levels(data$sedation_rea_type___1.factor)=c("Unchecked","Checked")
levels(data$sedation_rea_type___2.factor)=c("Unchecked","Checked")
levels(data$sedation_rea_type___3.factor)=c("Unchecked","Checked")
levels(data$sedation_rea_type___4.factor)=c("Unchecked","Checked")
levels(data$sedation_rea_type___5.factor)=c("Unchecked","Checked")
levels(data$trachetomie.factor)=c("Oui","Non")
levels(data$tbes_deglutition.factor)=c("Oui","Non")
levels(data$gpe.factor)=c("Oui","Non")
levels(data$lata.factor)=c("Oui","Non")
levels(data$ata.factor)=c("Oui","Non")
levels(data$deces_rea.factor)=c("Oui","Non")
levels(data$edme.factor)=c("Oui","Non")
levels(data$pmo.factor)=c("Oui","Non")
levels(data$pmo_maastricht.factor)=c("Arrêt des thérapeutiques actives","État de mort encéphalique")
levels(data$edme_confirmation.factor)=c("Angioscanner","EEG","Arteriographie")
levels(data$prise_en_charge_en_reanimation_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$sortie_rea.factor)=c("Autre reanimation","Service de neurochirurgie","Autre service de chirurgie","Service de neurologie","Autre service de medecine","SRPR","SMR","USPC / MAS","Hospitalisation à domicile (HAD)","Hospitalisation de jour (HDJ)","Domicile","Autre")
levels(data$ouverture_yeux.factor)=c("Oui","Non")
levels(data$etat_conscience.factor)=c("Eveil non repondant","Conscience minimale","Conscience")
levels(data$crsr.factor)=c("Oui","Non")
levels(data$eval_distance.factor)=c("Oui","Non")
levels(data$evaluation_distance_deces.factor)=c("Oui","Non")
levels(data$evaluation_distance_rad.factor)=c("Oui","Non")
levels(data$evaluation_distance_pec_spe.factor)=c("Oui","Non")
levels(data$eval_distance_score___1.factor)=c("Unchecked","Checked")
levels(data$eval_distance_score___5.factor)=c("Unchecked","Checked")
levels(data$eval_distance_score___2.factor)=c("Unchecked","Checked")
levels(data$eval_distance_score___3.factor)=c("Unchecked","Checked")
levels(data$eval_distance_score___6.factor)=c("Unchecked","Checked")
levels(data$eval_distance_score___4.factor)=c("Unchecked","Checked")
levels(data$donnees_a_la_suite_de_la_reanimation_complete.factor)=c("Incomplete","Unverified","Complete")
