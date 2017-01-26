# -*- coding: utf-8 -*-
# generating rules for QA1 operations

#  operation_id
f = open("QA.csv", "w")
# всего 24 операция по контролю качества
for i in range(1, 25):
    # 24 варианта операций
    for valve_bellow_id in range(1,3):
        # 1 - с сильфоном 2 - без сильфона
        for hydro_id in range(1, 4):
            # 1 - гидравлич. испытания проводятся, 2 - гидравлич. испытан   ия не проводятся
            for rad_id in range (1,3):
                # 1 - РГК проводится, 2 - РГК не проводится
                for fp_id in range(1, 4):
                    # контроль содержания ферритной фазы, если 1 - проводится при выполнении прочих условий, 2 - не проводится всегда:крышка 3 - седло задвижки
                    for tempr_id in range(1, 3):
                        # температура больше 100 если да - 1, если нет - 2
                        for tempr_operation_id in range(1, 3):
                            # температура рвнешняя больще 20 если да - 1, если нет - 2
                            for pressure_id in range(1, 3):
                                # давление если SP - 1, если NSP - 2
                                for valve_type_by_socet in range(1, 4):
                                    # тип клапана 1 - с перех.патрубком, 2 - без перех.патрубка 3 - задвижка
                                    for material_type_separete_id in range(1, 9):
                                            # 8 типо материалов (крепёж, отливка, поковка)
                                            for valve_qa_type_id in range(1, 11):
                                                # 10 классов безопасности
                                                # 101
                                                # 11 101__Химический анализ основного металла_+c_
                                                if i is 1:
                                                    operation_id = 1
                                                # 201 [2:5]
                                                # 2_201_Испытание на растяжение при нормальной температуре_Определение временного сопротивления при нормальной температуре_+c_
                                                # 46_201_Испытание на растяжение при нормальной температуре_Определение временного сопротивления при нормальной температуре_-_
                                                elif i is 2:
                                                    # крепёж гайки
                                                    if material_type_separete_id is 7 and valve_qa_type_id > 6:
                                                        operation_id = 46
                                                    else:
                                                        operation_id = 2
                                                # 3_201_Испытание на растяжение при нормальной температуре_Определение предела текучести при нормальной температуре_+c_
                                                # 47_201_Испытание на растяжение при нормальной температуре_Определение предела текучести при нормальной температуре_-_
                                                elif i is 3:
                                                    # крепёж гайки
                                                    if material_type_separete_id is 7 and valve_qa_type_id > 6:
                                                        operation_id = 47
                                                    else:
                                                        operation_id = 3
                                                # 4_201_Испытание на растяжение при нормальной температуре_Определение относительного удлинения при нормальной температуре_+c_
                                                # 48_201_Испытание на растяжение при нормальной температуре_Определение относительного удлинения при нормальной температуре_-_
                                                elif i is 4:
                                                    # крепёж гайки
                                                    if material_type_separete_id is 7 and valve_qa_type_id > 6:
                                                        operation_id = 48
                                                    else:
                                                        operation_id = 4
                                                # 5_201_Испытание на растяжение при нормальной температуре_Определение относительного сужения при нормальной температуре_+c_
                                                # 49_201_Испытание на растяжение при нормальной температуре_Определение относительного сужения при нормальной температуре_-_
                                                elif i is 5:
                                                    # крепёж гайки
                                                    if material_type_separete_id is 7 and valve_qa_type_id > 6:
                                                        operation_id = 49
                                                    else:
                                                        operation_id = 5
                                                # 206 [6:9]
                                                # 6_206_Испытание на растяжение при повышенной температуре_Определение временного сопротивления при повышенной температуре_+c_
                                                # 10_206_Испытание на растяжение при повышенной температуре_Определение временного сопротивления при повышенной температуре_-_
                                                elif i is 6:
                                                    if tempr_id is 1:
                                                        # крепёж
                                                        if valve_qa_type_id > 6 and 5 < material_type_separete_id < 8:
                                                            operation_id = 10
                                                        elif valve_qa_type_id > 7:
                                                            operation_id = 10
                                                        else:
                                                            operation_id = 6
                                                    else:
                                                        operation_id = 10
                                                # 7_206_Испытание на растяжение при повышенной температуре_Определение предела текучести при повышенной температуре_+c_
                                                # 11_206_Испытание на растяжение при повышенной температуре_Определение предела текучести при повышенной температуре_-_
                                                elif i is 7:
                                                    if tempr_id is 1:
                                                        if valve_qa_type_id > 6 and 5 < material_type_separete_id < 8:
                                                            operation_id = 11
                                                        elif valve_qa_type_id > 7:
                                                            operation_id = 11
                                                        else:
                                                            operation_id = 7
                                                    else:
                                                        operation_id = 11
                                                # 8_206_Испытание на растяжение при повышенной температуре_Определение относительного удлинения при повышенной температуре_+c_
                                                # 12_206_Испытание на растяжение при повышенной температуре_Определение относительного удлинения при повышенной температуре_-_
                                                elif i is 8:
                                                    if tempr_id is 1:
                                                        if valve_qa_type_id > 6 and 5 < material_type_separete_id < 8:
                                                            operation_id = 12
                                                        elif material_type_separete_id < 3:
                                                            operation_id = 12
                                                        elif valve_qa_type_id > 7:
                                                            operation_id = 12
                                                        else:
                                                            operation_id = 8
                                                    else:
                                                        operation_id = 12
                                                # 9_206_Испытание на растяжение при повышенной температуре_Определение относительного сужения при повышенной температуре_+c_
                                                # 13_206_Испытание на растяжение при повышенной температуре_Определение относительного сужения при повышенной температуре_-_
                                                elif i is 9:
                                                    if tempr_id is 1:
                                                        # крепёж
                                                        if valve_qa_type_id > 6 and 5 < material_type_separete_id < 8:
                                                            operation_id = 13
                                                        # поковка
                                                        elif valve_qa_type_id > 7 and (2 < material_type_separete_id < 6
                                                                                       or material_type_separete_id is 8):
                                                            operation_id = 13
                                                        # отливка
                                                        elif valve_qa_type_id > 6 and material_type_separete_id < 3:
                                                            operation_id = 13
                                                        else:
                                                            operation_id = 9
                                                    else:
                                                        operation_id = 13
                                                # 211
                                                # 14_211__Испытание на ударный изгиб при нормальной температуре_+c_
                                                # 15_211__Испытание на ударный изгиб при нормальной температуре_-_
                                                # 8,Поковка.Дупл.4,Поковка.НЖ
                                                # done + для всех отливок и + для поковок(кроме НЖ и дупл) в зависимости от клсса.
                                                elif i is 10:
                                                    # отливки
                                                    if material_type_separete_id < 3 and valve_qa_type_id < 10:
                                                        operation_id = 14
                                                    # поковки март и угл
                                                    elif (material_type_separete_id is 3 or material_type_separete_id is 5) \
                                                            and valve_qa_type_id < 9:
                                                        operation_id = 14
                                                    #    шпильки
                                                    elif material_type_separete_id is 6 and valve_qa_type_id < 10:
                                                        operation_id = 14
                                                    #     гайки
                                                    elif material_type_separete_id is 7 and valve_qa_type_id < 7:
                                                        operation_id = 14
                                                    else:
                                                        operation_id = 15
                                                # 216
                                                # 16_216__Определение или подтверждение критической температуры хрупкости_+c_
                                                # 17_216__Определение или подтверждение критической температуры хрупкости_-_
                                                elif i is 11:
                                                    # крепёж
                                                    if tempr_operation_id is 1:
                                                        operation_id = 17
                                                    elif 4 < material_type_separete_id < 8:
                                                        operation_id = 17
                                                    elif valve_qa_type_id > 7:
                                                        operation_id = 17
                                                    else:
                                                        operation_id = 16
                                                # 229
                                                # 18_229__Контроль содержания неметаллических включений_+c_
                                                # 19_229__Контроль содержания неметаллических включений_-_
                                                elif i is 12:
                                                    if (material_type_separete_id is 8 or material_type_separete_id is 4 or material_type_separete_id is 5) \
                                                            and valve_qa_type_id < 7:
                                                        operation_id = 18
                                                    else:
                                                        operation_id = 19
                                                # 231
                                                # 20_231__Контроль макроструктуры основного металла_+c_
                                                # 21_231__Контроль макроструктуры основного металла_-_
                                                elif i is 13:
                                                    if valve_qa_type_id < 8 and material_type_separete_id > 2:
                                                        operation_id = 20
                                                    elif 5 < material_type_separete_id < 8 and valve_qa_type_id < 10:
                                                        operation_id = 20
                                                    else:
                                                        operation_id = 21
                                                # 232
                                                # 22_232__Контроль твердости_+c_
                                                # 23_232__Контроль твердости_-_
                                                # done оставить +с только для отливок и крепежа в зависимости от класса безопасности
                                                elif i is 14:
                                                    # отливки
                                                    if material_type_separete_id < 3 and (valve_qa_type_id < 3
                                                                                          or valve_qa_type_id is 4):
                                                        operation_id = 22
                                                    # крепёж
                                                    elif (material_type_separete_id is 6 or material_type_separete_id is 7) \
                                                            and valve_qa_type_id < 10:
                                                        operation_id = 22
                                                    else:
                                                        operation_id = 23
                                                # 241
                                                # 24_241__Испытание на стойкость против межкристаллитной коррозии_+c_
                                                # 25_241__Испытание на стойкость против межкристаллитной коррозии_-_
                                                elif i is 15:
                                                    if material_type_separete_id is 2 or material_type_separete_id is 4\
                                                            or material_type_separete_id is 8:
                                                        operation_id = 24
                                                    else:
                                                        operation_id = 25
                                                   # 313
                                                # 26_313__Радиографический контроль кромок литых деталей под сварку_+c_
                                                # 27_313__Радиографический контроль кромок литых деталей под сварку_-_
                                                elif i is 16:
                                                    if rad_id is 1 and valve_type_by_socet is not 1 and material_type_separete_id < 3:
                                                        operation_id = 26
                                                    else:
                                                        operation_id = 27
                                                # 314
                                                # 28_314__Радиографический контроль отливок_100*100 %_100% изделий на 100% зон
                                                # 29_314__Радиографический контроль отливок_100% K3_100% изделий критических зон
                                                # 30_314__Радиографический контроль отливок_20% K3_20% изделий критических зон
                                                # 31_314__Радиографический контроль отливок_-_
                                                elif i is 17:
                                                    if material_type_separete_id < 3:
                                                        # SP pressure type
                                                        if pressure_id is 1:
                                                            if valve_qa_type_id < 3 or valve_qa_type_id is 4:
                                                                operation_id = 28
                                                            elif valve_qa_type_id is 3 or valve_qa_type_id is 5:
                                                                operation_id = 105
                                                            elif valve_qa_type_id is 6 or valve_qa_type_id is 7:
                                                                operation_id = 29
                                                            else:
                                                                operation_id = 31
                                                        # NPS pressure type
                                                        else:
                                                            if valve_qa_type_id is 1:
                                                                operation_id = 28
                                                            elif valve_qa_type_id < 7:
                                                                operation_id = 29
                                                            elif valve_qa_type_id is 7:
                                                                operation_id = 30
                                                            else:
                                                                operation_id = 31
                                                    else:
                                                        operation_id = 31
                                                # 326
                                                # 32_326__Ультразвуковой контроль основного металла_+c_
                                                # 33_326__Ультразвуковой контроль основного металла_-_
                                                elif i is 18:
                                                    if 5 < material_type_separete_id < 8 and valve_qa_type_id > 6:
                                                        operation_id = 33
                                                    elif material_type_separete_id < 3:
                                                        operation_id = 33
                                                    elif valve_qa_type_id > 7:
                                                        operation_id = 33
                                                    else:
                                                        operation_id = 32
                                                # 331
                                                # 34_331__Магнитопорошковый контроль_+_
                                                # 35_331__Магнитопорошковый контроль_-_
                                                elif i is 19:
                                                    if 5 < material_type_separete_id < 8:
                                                        operation_id = 35
                                                    else:
                                                        operation_id = 34
                                                # 341
                                                # 36_341__Капиллярный контроль_+_
                                                # 37_341__Капиллярный контроль_-_
                                                # 104;Капиллярный контроль;+**
                                                # done +** для гаек 1 и второго класса (**испытания проводятся для DN гайки больше или равно 110мм
                                                elif i is 20:
                                                    # все +
                                                    if material_type_separete_id is not 6 and \
                                                                    material_type_separete_id is not 7:
                                                        operation_id = 36
                                                    # гайки 1 и 2 класса +**
                                                    elif material_type_separete_id is 7 and valve_qa_type_id < 7:
                                                        operation_id = 104
                                                    # шпильки 1 и 2 класса +
                                                    elif material_type_separete_id is 6 and valve_qa_type_id < 7:
                                                        operation_id = 36
                                                    # остальные -
                                                    else:
                                                        operation_id = 37
                                                # 351
                                                # 38_351__Контроль содержания ферритной фазы_+c_
                                                # 39_351__Контроль содержания ферритной фазы_-_
                                                # done проводится только для деталей под сварку: СИЛЬФОН ВЕСЬ, корпус,патрубок,седло задвижки фланец
                                                # done только для НЖ(пок/отл)+Дупл, весь сильфон,корпус,патрубок,фланец, И ЕСЛИ ЗАДВИЖКА то и седло
                                                elif i is 21:
                                                    # сильфон/корпус/патрубок/фланец
                                                    if fp_id is 1 and \
                                                            (material_type_separete_id is 2 or material_type_separete_id is 4 \
                                                            or material_type_separete_id is 8):
                                                        operation_id = 38
                                                    # седло задвижки
                                                    elif fp_id is 3 and valve_type_by_socet is 3 and \
                                                            (material_type_separete_id is 2 or material_type_separete_id is 4 \
                                                            or material_type_separete_id is 8):
                                                        operation_id = 38
                                                    else:
                                                        operation_id = 39
                                                # 411
                                                # 40_411__Контроль проведения термической обработки_+c_
                                                # 41_411__Контроль проведения термической обработки_-_
                                                elif i is 22:
                                                    operation_id = 40
                                                # 421
                                                # 42_421__Гидравлические испытания_+_
                                                # 43_421__Гидравлические испытания_-_
                                                # done уточнить что есть "сильфон" для данного вида испытаний (+шток + фланец)
                                                # done шток гидравл. только с сильфоном
                                                elif i is 23:
                                                    if hydro_id is 1:
                                                        operation_id = 42
                                                    elif hydro_id is 3 and valve_bellow_id is 1:
                                                        operation_id = 42
                                                    else:
                                                        operation_id = 43
                                                # 445
                                                # 44_445__Контроль качества защитных покрытий_+c_
                                                # 45_445__Контроль качества защитных покрытий_-_
                                                elif i is 24:
                                                    if 5 < material_type_separete_id < 8:
                                                        operation_id = 44
                                                    else:
                                                        operation_id = 45
                                                else:
                                                    operation_id = 111
                                                str_out = str(tempr_id) + "," + str(tempr_operation_id) + "," + str(pressure_id) + "," + str(material_type_separete_id)\
                                                          + "," + str(valve_qa_type_id) + "," + str(operation_id) + "," + str(fp_id) + "," + str(hydro_id)\
                                                          + "," + str(valve_type_by_socet) + "," + str(rad_id) + "," + str(valve_bellow_id)
                                                f.write(str_out + '\n')

                                                # material_type_separete_id
                                                # 1,Отливка.Угл.
                                                # 2,Отливка.НЖ
                                                # 3,Поковка.Угл.
                                                # 4,Поковка.НЖ
                                                # 5,Поковка.Март.
                                                # 6,Крепёж.Шпильки
                                                # 7,Крепёж.Гайки
                                                # 8,Поковка.Дупл.
                                                # valve_qa_type_id
                                                # #1,1А
                                                # 2,2ВIIа
                                                # 3,2ВIIb
                                                # 4,2ВIIIа
                                                # 5,2ВIIIb
                                                # 6,2ВIIIс
                                                # 7,3СIIIа
                                                # 8,3CIIIb
                                                # 9,3СIIIс

