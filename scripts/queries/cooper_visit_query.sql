SELECT `person_id`, `visit_concept_id`, `concept_name`, `visit_start_date`, `visit_end_date`
FROM (SELECT `visit_occurrence_id`, `person_id`, `visit_concept_id`, `visit_start_date`, `visit_start_datetime`, `visit_end_date`, `visit_end_datetime`, `visit_type_concept_id`, `provider_id`, `care_site_id`, `visit_source_value`, `visit_source_concept_id`, `admitted_from_concept_id`, `admitted_from_source_value`, `discharge_to_source_value`, `discharge_to_concept_id`, `preceding_visit_occurrence_id`, `concept_name`, `domain_id`, `vocabulary_id`, `concept_class_id`, `standard_concept`, `concept_code`, `valid_start_date`, `valid_end_date`, `invalid_reason`
FROM (SELECT *
FROM `visit_occurrence`
WHERE (`person_id` IN (564162.0, 1593576.0, 1602063.0, 350015.0, 437418.0, 580489.0, 42063.0, 251155.0, 640865.0, 242861.0, 476135.0, 403120.0, 431449.0, 495209.0, 388518.0, 683229.0, 287731.0, 528400.0, 29618.0, 469612.0, 259949.0, 670919.0, 46319.0, 413840.0, NULL, 265252.0, 221802.0, 684829.0, 586335.0, 397816.0, 621710.0, 664350.0, 33706.0, 578679.0, 608684.0, 87233.0, 633604.0, 41937.0, 112635.0, 158133.0, 276171.0, 1058835.0, 848942.0, 619355.0, 1218603.0, 26467.0, 86011.0, 339017.0, 566911.0, 193203.0, 58656.0, 522494.0, 424055.0, 318960.0, 1615991.0, 497230.0, 14289.0, 214631.0, 417534.0, 620686.0, 661921.0, 234766.0, 29138.0, 537998.0, 142018.0, 387493.0, 553055.0, 583665.0, 19992.0, 244900.0, 283229.0, 428570.0, 974728.0, 354623.0, 221526.0, 213881.0, 869777.0, 547642.0, 218585.0, 529996.0, 508823.0, 122976.0, 482316.0, 483253.0, 472457.0, 519748.0, 508758.0, 432473.0, 613657.0, 1069513.0, 600007.0, 671516.0, 171689.0, 182770.0, 114095.0, 422901.0, 79024.0, 103415.0, 426385.0, 519780.0, 57157.0, 238009.0, 89520.0, 912879.0, 79490.0, 473050.0, 709095.0, 509911.0, 32257.0, 350358.0, 478185.0, 347186.0, 537145.0, 325891.0, 1648539.0, 345064.0, 108274.0, 381412.0, 547018.0, 664550.0, 642741.0, 701237.0, 84674.0, 409755.0, 527754.0, 323117.0, 619129.0, 84724.0, 354837.0, 602774.0, 345752.0, 528415.0, 577496.0, 286936.0, 286725.0, 89232.0, 706189.0, 576812.0, 314772.0, 127010.0, 486909.0, 150462.0, 186422.0, 582673.0, 262596.0, 200489.0, 14375.0, 697256.0, 748670.0, 132161.0, 532118.0, 541976.0, 526959.0, 482159.0, 82072.0, 115398.0, 50164.0, 559674.0, 304085.0, 149907.0, 691711.0, 29026.0, 417870.0, 548082.0, 27768.0, 36665.0, 392118.0, 121369.0, 377763.0, 800410.0, 641147.0, 425566.0, 214266.0, 608381.0, 199521.0, 470923.0, 371697.0, 408473.0, 225737.0, 247873.0, 471245.0, 675445.0, 676385.0, 80034.0, 585981.0, 308675.0, 690088.0, 8767.0, 172149.0, 602154.0, 564539.0, 633590.0, 53556.0, 140751.0, 486743.0, 692510.0, 638109.0, 437179.0, 187737.0, 464771.0, 424655.0, 373133.0, 613445.0, 535637.0, 693237.0, 460301.0, 98611.0, 708054.0, 431887.0, 1660955.0, 1661918.0, 12833.0, 257697.0, 572175.0, 153618.0, 70672.0, 231767.0, 696863.0, 618998.0, 466279.0, 407492.0, 354412.0, 651911.0, 694023.0, 475740.0, 656279.0, 491048.0, 36814.0, 644099.0, 388682.0, 298.0, 251324.0, 11249.0, 278596.0, 484267.0, 590322.0, 597739.0, 38509.0, 359064.0, 282669.0, 467358.0, 674615.0, 250013.0, 268030.0, 503818.0, 511558.0, 570631.0, 515011.0, 97690.0, 116467.0, 85423.0, 577372.0, 427207.0, 233350.0, 394356.0, 468892.0, 196566.0, 553523.0, 135966.0, 501373.0, 269869.0, 488416.0, 236451.0, 199071.0, 208612.0, 227088.0, 29263.0, 1211682.0, 358036.0, 100537.0, 134910.0, 2363.0, 368523.0, 703586.0, 345072.0, 36501.0, 622800.0, 255871.0, 545953.0, 173702.0, 136255.0, 396277.0, 170985.0, 188377.0, 85488.0, 659749.0, 124435.0, 207641.0, 594910.0, 473611.0, 575279.0, 685998.0, 463852.0, 566178.0, 560653.0, 424706.0, 36900.0, 553298.0, 583428.0, 76803.0, 658015.0, 95689.0, 297590.0, 650439.0, 666599.0, 226034.0, 354555.0, 638959.0, 340011.0, 615582.0, 657307.0, 454872.0, 260968.0, 457901.0, 262370.0, 603675.0, 305214.0, 520677.0, 603390.0, 327537.0, 420721.0, 317064.0, 361614.0, 88205.0, 516091.0, 267435.0, 568154.0, 106781.0, 158396.0, 487872.0, 344604.0, 487503.0, 347845.0, 925182.0, 449696.0, 610462.0, 767246.0, 1850.0, 105145.0, 494710.0, 290485.0, 400794.0, 309795.0, 162176.0, 421205.0, 1661155.0, 241479.0, 396925.0, 2400.0, 517256.0, 152862.0, 407921.0, 163859.0, 191540.0, 78580.0, 674094.0, 47211.0, 592571.0, 388375.0, 60623.0, 43967.0, 575849.0, 200672.0, 1081384.0, 32167.0, 119631.0, 309562.0, 432985.0, 416686.0, 644031.0, 619131.0, 517301.0, 586888.0, 297945.0, 191796.0, 113409.0, 307655.0, 646991.0, 172155.0, 239603.0, 929442.0, 108349.0, 452344.0, 436533.0, 500889.0, 671260.0, 430033.0, 90287.0, 528295.0, 430649.0, 91093.0, 165274.0, 434675.0, 587910.0, 630480.0, 313210.0, 599264.0, 394773.0, 638129.0, 491709.0, 1660604.0, 1022806.0, 609635.0, 627852.0, 138881.0, 223141.0, 142598.0, 133285.0, 576103.0, 648164.0, 968619.0, 311412.0, 25036.0, 12777.0, 1222073.0, 417383.0, 39948.0, 1662000.0, 1245879.0, 387272.0, 405991.0, 111319.0, 51031.0, 99227.0, 507305.0, 235790.0, 697675.0, 574606.0, 400866.0, 346768.0, 385579.0, 311297.0, 274491.0, 669820.0, 526138.0, 236534.0, 652065.0, 394725.0, 601557.0, 174568.0, 231159.0, 422744.0, 153979.0, 806224.0, 349982.0, 607665.0, 705732.0, 428352.0, 126697.0, 41809.0, 443898.0, 188733.0, 387549.0, 23402.0, 257228.0, 1661309.0, 388896.0, 333537.0, 55544.0, 1221505.0, 531227.0, 17196.0, 730746.0, 483816.0, 338177.0, 652258.0, 709016.0, 1661761.0, 303182.0, 684601.0, 158999.0, 466733.0, 216027.0, 459955.0, 7756.0, 109636.0, 180288.0, 582970.0, 237968.0, 35182.0, 455630.0, 487501.0, 71891.0, 322275.0, 679406.0, 337698.0, 157604.0, 46572.0, 15227.0, 156383.0, 633057.0, 580130.0, 568044.0, 29392.0, 674519.0, 190801.0, 551720.0, 8396661.0, 844672.0, 98888.0, 104818.0, 394467.0, 410761.0, 444572.0, 196721.0, 96715.0, 1662017.0, 207269.0, 495549.0, 201565.0, 306846.0, 222815.0, 10013.0, 684179.0, 271486.0, 509550.0, 204843.0, 209782.0, 1661550.0, 321655.0, 13814.0, 14038.0, 1661527.0, 217377.0, 494671.0, 114920.0, 526223.0, 641280.0, 193169.0, 261283.0, 540557.0, 97826.0, 310538.0, 128767.0, 1056424.0, 481709.0, 434664.0, 67763.0, 659279.0, 579328.0, 318713.0, 12895.0, 101128.0, 238609.0, 69859.0, 623367.0, 192836.0, 414456.0, 519227.0, 68766.0, 78698.0, 421821.0, 455987.0, 11117.0, 659288.0, 630986.0, 42480.0, 659742.0, 292186.0, 672477.0, 560101.0, 355757.0, 552362.0, 510595.0, 1661507.0, 327747.0, 845.0, 48659.0, 332602.0, 619787.0, 483196.0, 54427.0, 636154.0, 630239.0, 275860.0, 1278223.0, 214707.0, 620416.0, 507294.0))) `LHS`
LEFT JOIN (SELECT *
FROM `concept`
WHERE (`domain_id` = 'Visit')) `RHS`
ON (`LHS`.`visit_concept_id` = `RHS`.`concept_id`)
) `q01`