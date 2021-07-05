Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t1"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "x"
                    , type_ = Just "INTEGER"
                    , constraints =
                        [ Named
                            { name = Nothing
                            , value = ColumnConstraint'Check
                                ( Expression'LessThan
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "x"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "5" )
                                    )
                                )
                            }
                        ]
                    } :|
                    [ ColumnDefinition
                        { name = "y"
                        , type_ = Just "REAL"
                        , constraints =
                            [ Named
                                { name = Nothing
                                , value = ColumnConstraint'Check
                                    ( Expression'GreaterThan
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "y"
                                                }
                                            )
                                        )
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "x"
                                                }
                                            )
                                        )
                                    )
                                }
                            ]
                        }
                    ]
                , constraints = []
                , withoutRowid = False
                }
            )
        }
    )