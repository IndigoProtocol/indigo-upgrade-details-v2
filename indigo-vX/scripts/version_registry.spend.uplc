(program
  1.0.0
  [
    (lam
      i_0
      [
        (lam
          i_1
          [
            (lam
              i_2
              [
                (lam
                  i_3
                  [
                    (lam
                      i_4
                      [
                        (lam
                          i_5
                          (lam
                            i_6
                            [
                              (lam
                                i_7
                                (lam
                                  i_8
                                  (lam
                                    i_9
                                    (force
                                      [
                                        [
                                          [ i_5 (con bool False) ]
                                          (delay (con unit ()))
                                        ]
                                        (delay [ (error ) (force (error )) ])
                                      ]
                                    )
                                  )
                                )
                              )
                              (force
                                [
                                  [
                                    [
                                      i_5
                                      [
                                        [
                                          (builtin equalsInteger)
                                          (con integer 0)
                                        ]
                                        [ i_4 [ (builtin unConstrData) i_6 ] ]
                                      ]
                                    ]
                                    (delay
                                      [
                                        (lam
                                          i_10
                                          [
                                            (lam
                                              i_11
                                              [
                                                (lam
                                                  i_12
                                                  [
                                                    (lam
                                                      i_13
                                                      (force
                                                        [
                                                          [
                                                            [ i_2 [ i_0 i_12 ] ]
                                                            (delay
                                                              [
                                                                (lam
                                                                  i_14
                                                                  (con unit ())
                                                                )
                                                                [
                                                                  (lam
                                                                    i_15
                                                                    [
                                                                      [
                                                                        i_15
                                                                        i_15
                                                                      ]
                                                                      i_13
                                                                    ]
                                                                  )
                                                                  (lam
                                                                    i_16
                                                                    (lam
                                                                      i_17
                                                                      (force
                                                                        [
                                                                          [
                                                                            [
                                                                              i_2
                                                                              i_17
                                                                            ]
                                                                            (delay
                                                                              (con
                                                                                unit
                                                                                ()
                                                                              )
                                                                            )
                                                                          ]
                                                                          (delay
                                                                            [
                                                                              (lam
                                                                                i_18
                                                                                [
                                                                                  [
                                                                                    i_16
                                                                                    i_16
                                                                                  ]
                                                                                  [
                                                                                    i_0
                                                                                    i_17
                                                                                  ]
                                                                                ]
                                                                              )
                                                                              [
                                                                                (lam
                                                                                  i_19
                                                                                  [
                                                                                    (lam
                                                                                      i_20
                                                                                      [
                                                                                        (builtin
                                                                                          unBData
                                                                                        )
                                                                                        [
                                                                                          i_3
                                                                                          i_19
                                                                                        ]
                                                                                      ]
                                                                                    )
                                                                                    [
                                                                                      (builtin
                                                                                        unBData
                                                                                      )
                                                                                      [
                                                                                        i_4
                                                                                        i_19
                                                                                      ]
                                                                                    ]
                                                                                  ]
                                                                                )
                                                                                [
                                                                                  i_1
                                                                                  i_17
                                                                                ]
                                                                              ]
                                                                            ]
                                                                          )
                                                                        ]
                                                                      )
                                                                    )
                                                                  )
                                                                ]
                                                              ]
                                                            )
                                                          ]
                                                          (delay (error ))
                                                        ]
                                                      )
                                                    )
                                                    [
                                                      (builtin unMapData)
                                                      [ i_1 i_12 ]
                                                    ]
                                                  ]
                                                )
                                                [ i_0 i_10 ]
                                              ]
                                            )
                                            [ (builtin unIData) [ i_1 i_10 ] ]
                                          ]
                                        )
                                        [ i_3 [ (builtin unConstrData) i_6 ] ]
                                      ]
                                    )
                                  ]
                                  (delay (error ))
                                ]
                              )
                            ]
                          )
                        )
                        (force (builtin ifThenElse))
                      ]
                    )
                    (force (force (builtin fstPair)))
                  ]
                )
                (force (force (builtin sndPair)))
              ]
            )
            (force (force (builtin chooseList)))
          ]
        )
        (force (builtin headList))
      ]
    )
    (force (builtin tailList))
  ]
)