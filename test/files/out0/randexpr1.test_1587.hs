Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Divide
                                ( Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "abs"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'Plus
                                                    ( Expression'Divide
                                                        ( Expression'FunctionCall
                                                            ( FunctionCallExpression
                                                                { call = FunctionCall
                                                                    { name = Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "abs"
                                                                        }
                                                                    , arguments = FunctionArguments'Arguments
                                                                        [ Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "a"
                                                                                }
                                                                            )
                                                                        ]
                                                                    }
                                                                , filter = Nothing
                                                                , over = Nothing
                                                                }
                                                            )
                                                        )
                                                        ( Expression'FunctionCall
                                                            ( FunctionCallExpression
                                                                { call = FunctionCall
                                                                    { name = Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "abs"
                                                                        }
                                                                    , arguments = FunctionArguments'Arguments
                                                                        [ Expression'Plus
                                                                            ( Expression'Minus
                                                                                ( Expression'Plus
                                                                                    ( Expression'BitwiseNegate
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "17" )
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Just
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "t1"
                                                                                                    }
                                                                                                )
                                                                                            , value = "b"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                )
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "e"
                                                                                        }
                                                                                    )
                                                                                )
                                                                            )
                                                                            ( Expression'Divide
                                                                                ( Expression'FunctionCall
                                                                                    ( FunctionCallExpression
                                                                                        { call = FunctionCall
                                                                                            { name = Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "abs"
                                                                                                }
                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                [ Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "a"
                                                                                                        }
                                                                                                    )
                                                                                                ]
                                                                                            }
                                                                                        , filter = Nothing
                                                                                        , over = Nothing
                                                                                        }
                                                                                    )
                                                                                )
                                                                                ( Expression'FunctionCall
                                                                                    ( FunctionCallExpression
                                                                                        { call = FunctionCall
                                                                                            { name = Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "abs"
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
                                                                                                                [ Expression'Subquery
                                                                                                                    ( SelectStatement
                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                        , select = CompoundSelect
                                                                                                                            ( SelectCore'Select
                                                                                                                                ( Select
                                                                                                                                    { distinct = False
                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                        ( Aliased
                                                                                                                                            { value = Expression'FunctionCall
                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                    { call = FunctionCall
                                                                                                                                                        { name = Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "max"
                                                                                                                                                            }
                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                            [ Expression'LiteralValue
                                                                                                                                                                ( Number "13" )
                                                                                                                                                            ]
                                                                                                                                                        }
                                                                                                                                                    , filter = Nothing
                                                                                                                                                    , over = Nothing
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            , alias = Nothing
                                                                                                                                            }
                                                                                                                                        ) :| []
                                                                                                                                    , from = Just
                                                                                                                                        ( Table
                                                                                                                                            ( QualifiedTableName
                                                                                                                                                { name = Aliased
                                                                                                                                                    { value = Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "t1"
                                                                                                                                                        }
                                                                                                                                                    , alias = Nothing
                                                                                                                                                    }
                                                                                                                                                , indexedBy = Nothing
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    , where_ = Just
                                                                                                                                        ( Expression'Not
                                                                                                                                            ( Expression'InValues
                                                                                                                                                ( InValuesExpression
                                                                                                                                                    { expression = Expression'Column
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "c"
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    , values =
                                                                                                                                                        [ Expression'Plus
                                                                                                                                                            ( Expression'Minus
                                                                                                                                                                ( Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "c"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                                ( Expression'Divide
                                                                                                                                                                    ( Expression'FunctionCall
                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "abs"
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
                                                                                                                                                                                                    [ Expression'Subquery
                                                                                                                                                                                                        ( SelectStatement
                                                                                                                                                                                                            { commonTableExpressions = Nothing
                                                                                                                                                                                                            , select = CompoundSelect
                                                                                                                                                                                                                ( SelectCore'Select
                                                                                                                                                                                                                    ( Select
                                                                                                                                                                                                                        { distinct = False
                                                                                                                                                                                                                        , columns = ResultColumn'Expression
                                                                                                                                                                                                                            ( Aliased
                                                                                                                                                                                                                                { value = Expression'FunctionCall
                                                                                                                                                                                                                                    ( FunctionCallExpression
                                                                                                                                                                                                                                        { call = FunctionCall
                                                                                                                                                                                                                                            { name = Namespaced
                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                , value = "max"
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                [ Expression'Minus
                                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                            , value = "b"
                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                            , value = "f"
                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                ]
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        , filter = Nothing
                                                                                                                                                                                                                                        , over = Nothing
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                , alias = Nothing
                                                                                                                                                                                                                                }
                                                                                                                                                                                                                            ) :| []
                                                                                                                                                                                                                        , from = Just
                                                                                                                                                                                                                            ( Table
                                                                                                                                                                                                                                ( QualifiedTableName
                                                                                                                                                                                                                                    { name = Aliased
                                                                                                                                                                                                                                        { value = Namespaced
                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        , alias = Nothing
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    , indexedBy = Nothing
                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                        , where_ = Just
                                                                                                                                                                                                                            ( Expression'GreaterThan
                                                                                                                                                                                                                                ( Expression'Minus
                                                                                                                                                                                                                                    ( Expression'Divide
                                                                                                                                                                                                                                        ( Expression'FunctionCall
                                                                                                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                        , value = "abs"
                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                        [ Expression'Minus
                                                                                                                                                                                                                                                            ( Expression'Plus
                                                                                                                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                                                                                                                    ( Number "13" )
                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                ( Expression'Case
                                                                                                                                                                                                                                                                    ( CaseExpression
                                                                                                                                                                                                                                                                        { base = Just
                                                                                                                                                                                                                                                                            ( Expression'BitwiseNegate
                                                                                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                        , cases =
                                                                                                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                    , value = "c"
                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                            , Expression'Column
                                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                    , value = "b"
                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                            ) :| []
                                                                                                                                                                                                                                                                        , else_ = Expression'Column
                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                , value = "b"
                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                    , value = "e"
                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                , filter = Nothing
                                                                                                                                                                                                                                                , over = Nothing
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                        ( Expression'FunctionCall
                                                                                                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                        , value = "abs"
                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                        [ Expression'Column
                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                , value = "c"
                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                , filter = Nothing
                                                                                                                                                                                                                                                , over = Nothing
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                            , value = "e"
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        , value = "c"
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                        , groupBy = Nothing
                                                                                                                                                                                                                        , window = Nothing
                                                                                                                                                                                                                        }
                                                                                                                                                                                                                    )
                                                                                                                                                                                                                )
                                                                                                                                                                                                            , orderBy = Nothing
                                                                                                                                                                                                            , limit = Nothing
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
                                                                                                                                                                                                    , Expression'Column
                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                )
                                                                                                                                                                                                            , value = "c"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
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
                                                                                                                                                                    ( Expression'FunctionCall
                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "abs"
                                                                                                                                                                                    }
                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                    [ Expression'LiteralValue
                                                                                                                                                                                        ( Number "11" )
                                                                                                                                                                                    ]
                                                                                                                                                                                }
                                                                                                                                                                            , filter = Nothing
                                                                                                                                                                            , over = Nothing
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                            ( Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Just
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "t1"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    , value = "b"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        , Expression'Column
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Just
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "t1"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                , value = "a"
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        , Expression'LiteralValue
                                                                                                                                                            ( Number "11" )
                                                                                                                                                        ]
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    , groupBy = Nothing
                                                                                                                                    , window = Nothing
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            )
                                                                                                                        , orderBy = Nothing
                                                                                                                        , limit = Nothing
                                                                                                                        }
                                                                                                                    )
                                                                                                                , Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Just
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "t1"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        , value = "a"
                                                                                                                        }
                                                                                                                    )
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
                                                                            )
                                                                        ]
                                                                    }
                                                                , filter = Nothing
                                                                , over = Nothing
                                                                }
                                                            )
                                                        )
                                                    )
                                                    ( Expression'LiteralValue
                                                        ( Number "19" )
                                                    )
                                                ]
                                            }
                                        , filter = Nothing
                                        , over = Nothing
                                        }
                                    )
                                )
                                ( Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "abs"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'Negate
                                                    ( Expression'Negate
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Just
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "t1"
                                                                        }
                                                                    )
                                                                , value = "c"
                                                                }
                                                            )
                                                        )
                                                    )
                                                ]
                                            }
                                        , filter = Nothing
                                        , over = Nothing
                                        }
                                    )
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "t1"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'Not
                            ( Expression'And
                                ( Expression'GreaterThan
                                    ( Expression'Plus
                                        ( Expression'Plus
                                            ( Expression'BitwiseNegate
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Just
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "t1"
                                                                }
                                                            )
                                                        , value = "e"
                                                        }
                                                    )
                                                )
                                            )
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "t1"
                                                            }
                                                        )
                                                    , value = "a"
                                                    }
                                                )
                                            )
                                        )
                                        ( Expression'Divide
                                            ( Expression'FunctionCall
                                                ( FunctionCallExpression
                                                    { call = FunctionCall
                                                        { name = Namespaced
                                                            { namespace = Nothing
                                                            , value = "abs"
                                                            }
                                                        , arguments = FunctionArguments'Arguments
                                                            [ Expression'Case
                                                                ( CaseExpression
                                                                    { base = Nothing
                                                                    , cases =
                                                                        ( Expression'Equals
                                                                            ( Expression'Minus
                                                                                ( Expression'Minus
                                                                                    ( Expression'Plus
                                                                                        ( Expression'Plus
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "d"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "e"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                        ( Expression'Multiply
                                                                                            ( Expression'LiteralValue
                                                                                                ( Number "17" )
                                                                                            )
                                                                                            ( Expression'BitwiseNegate
                                                                                                ( Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Just
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "t1"
                                                                                                                }
                                                                                                            )
                                                                                                        , value = "a"
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Just
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "t1"
                                                                                                    }
                                                                                                )
                                                                                            , value = "a"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                )
                                                                                ( Expression'Multiply
                                                                                    ( Expression'Multiply
                                                                                        ( Expression'Multiply
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "a"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                            ( Expression'BitwiseNegate
                                                                                                ( Expression'Divide
                                                                                                    ( Expression'FunctionCall
                                                                                                        ( FunctionCallExpression
                                                                                                            { call = FunctionCall
                                                                                                                { name = Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "abs"
                                                                                                                    }
                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                    [ Expression'BitwiseNegate
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "f"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    ]
                                                                                                                }
                                                                                                            , filter = Nothing
                                                                                                            , over = Nothing
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                    ( Expression'FunctionCall
                                                                                                        ( FunctionCallExpression
                                                                                                            { call = FunctionCall
                                                                                                                { name = Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "abs"
                                                                                                                    }
                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                    [ Expression'Negate
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "13" )
                                                                                                                        )
                                                                                                                    ]
                                                                                                                }
                                                                                                            , filter = Nothing
                                                                                                            , over = Nothing
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                        ( Expression'BitwiseOr
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Just
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "t1"
                                                                                                            }
                                                                                                        )
                                                                                                    , value = "c"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "a"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Just
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "t1"
                                                                                                    }
                                                                                                )
                                                                                            , value = "b"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                )
                                                                            )
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "e"
                                                                                    }
                                                                                )
                                                                            )
                                                                        , Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Just
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "t1"
                                                                                        }
                                                                                    )
                                                                                , value = "d"
                                                                                }
                                                                            )
                                                                        ) :|
                                                                        [
                                                                            ( Expression'Not
                                                                                ( Expression'Between
                                                                                    ( Expression'Negate
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "e"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "c"
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Negate
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "d"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                )
                                                                            , Expression'Negate
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Just
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "t1"
                                                                                                }
                                                                                            )
                                                                                        , value = "f"
                                                                                        }
                                                                                    )
                                                                                )
                                                                            )
                                                                        ]
                                                                    , else_ = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Just
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "t1"
                                                                                    }
                                                                                )
                                                                            , value = "b"
                                                                            }
                                                                        )
                                                                    }
                                                                )
                                                            ]
                                                        }
                                                    , filter = Nothing
                                                    , over = Nothing
                                                    }
                                                )
                                            )
                                            ( Expression'FunctionCall
                                                ( FunctionCallExpression
                                                    { call = FunctionCall
                                                        { name = Namespaced
                                                            { namespace = Nothing
                                                            , value = "abs"
                                                            }
                                                        , arguments = FunctionArguments'Arguments
                                                            [ Expression'LiteralValue
                                                                ( Number "19" )
                                                            ]
                                                        }
                                                    , filter = Nothing
                                                    , over = Nothing
                                                    }
                                                )
                                            )
                                        )
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "t1"
                                                    }
                                                )
                                            , value = "f"
                                            }
                                        )
                                    )
                                )
                                ( Expression'LessThan
                                    ( Expression'LiteralValue
                                        ( Number "11" )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "11" )
                                    )
                                )
                            )
                        )
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )