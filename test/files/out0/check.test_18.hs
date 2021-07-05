Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t4"
            }
        , definition = Right
            ( TableDefinition
                { columns = ColumnDefinition
                    { name = "x"
                    , type_ = Nothing
                    , constraints = []
                    } :|
                    [ ColumnDefinition
                        { name = "y"
                        , type_ = Nothing
                        , constraints = []
                        }
                    ]
                , constraints =
                    [ Named
                        { name = Nothing
                        , value = TableConstraint'Check
                            ( Expression'Or
                                ( Expression'Or
                                    ( Expression'Or
                                        ( Expression'Equals
                                            ( Expression'Plus
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "x"
                                                        }
                                                    )
                                                )
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "y"
                                                        }
                                                    )
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "11" )
                                            )
                                        )
                                        ( Expression'Equals
                                            ( Expression'Multiply
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "x"
                                                        }
                                                    )
                                                )
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "y"
                                                        }
                                                    )
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "12" )
                                            )
                                        )
                                    )
                                    ( Expression'Between
                                        ( Expression'Divide
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "x"
                                                    }
                                                )
                                            )
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "y"
                                                    }
                                                )
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( Number "5" )
                                        )
                                        ( Expression'LiteralValue
                                            ( Number "8" )
                                        )
                                    )
                                )
                                ( Expression'Equals
                                    ( Expression'Negate
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "x"
                                                }
                                            )
                                        )
                                    )
                                    ( Expression'Plus
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "y"
                                                }
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( Number "10" )
                                        )
                                    )
                                )
                            )
                        }
                    ]
                , withoutRowid = False
                }
            )
        }
    )