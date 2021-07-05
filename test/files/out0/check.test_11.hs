Statement'CreateTable
    ( CreateTableStatement
        { temporary = False
        , ifNotExists = False
        , name = Namespaced
            { namespace = Nothing
            , value = "t2"
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
                                ( Expression'Equals
                                    ( Expression'FunctionCall
                                        ( FunctionCallExpression
                                            { call = FunctionCall
                                                { name = Namespaced
                                                    { namespace = Nothing
                                                    , value = "typeof"
                                                    }
                                                , arguments = FunctionArguments'Arguments
                                                    [ Expression'FunctionCall
                                                        ( FunctionCallExpression
                                                            { call = FunctionCall
                                                                { name = Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "coalesce"
                                                                    }
                                                                , arguments = FunctionArguments'Arguments
                                                                    [ Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "x"
                                                                            }
                                                                        )
                                                                    , Expression'LiteralValue
                                                                        ( Number "0" )
                                                                    ]
                                                                }
                                                            , filter = Nothing
                                                            , over = Nothing
                                                            }
                                                        )
                                                    ]
                                                }
                                            , filter = Nothing
                                            , over = Nothing
                                            }
                                        )
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "integer"
                                            }
                                        )
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
                                    ( Expression'Equals
                                        ( Expression'FunctionCall
                                            ( FunctionCallExpression
                                                { call = FunctionCall
                                                    { name = Namespaced
                                                        { namespace = Nothing
                                                        , value = "typeof"
                                                        }
                                                    , arguments = FunctionArguments'Arguments
                                                        [ Expression'FunctionCall
                                                            ( FunctionCallExpression
                                                                { call = FunctionCall
                                                                    { name = Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "coalesce"
                                                                        }
                                                                    , arguments = FunctionArguments'Arguments
                                                                        [ Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "y"
                                                                                }
                                                                            )
                                                                        , Expression'LiteralValue
                                                                            ( Number "0.1" )
                                                                        ]
                                                                    }
                                                                , filter = Nothing
                                                                , over = Nothing
                                                                }
                                                            )
                                                        ]
                                                    }
                                                , filter = Nothing
                                                , over = Nothing
                                                }
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( String "real" )
                                        )
                                    )
                                }
                            ]
                        }
                    , ColumnDefinition
                        { name = "z"
                        , type_ = Just "TEXT"
                        , constraints =
                            [ Named
                                { name = Nothing
                                , value = ColumnConstraint'Check
                                    ( Expression'Equals
                                        ( Expression'FunctionCall
                                            ( FunctionCallExpression
                                                { call = FunctionCall
                                                    { name = Namespaced
                                                        { namespace = Nothing
                                                        , value = "typeof"
                                                        }
                                                    , arguments = FunctionArguments'Arguments
                                                        [ Expression'FunctionCall
                                                            ( FunctionCallExpression
                                                                { call = FunctionCall
                                                                    { name = Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "coalesce"
                                                                        }
                                                                    , arguments = FunctionArguments'Arguments
                                                                        [ Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "z"
                                                                                }
                                                                            )
                                                                        , Expression'LiteralValue
                                                                            ( String "" )
                                                                        ]
                                                                    }
                                                                , filter = Nothing
                                                                , over = Nothing
                                                                }
                                                            )
                                                        ]
                                                    }
                                                , filter = Nothing
                                                , over = Nothing
                                                }
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( String "text" )
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