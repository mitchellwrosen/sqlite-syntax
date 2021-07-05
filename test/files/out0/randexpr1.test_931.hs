Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Minus
                                ( Expression'Minus
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "d"
                                            }
                                        )
                                    )
                                    ( Expression'FunctionCall
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
                                                                                { value = Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "d"
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
                                                                            ( Expression'InValues
                                                                                ( InValuesExpression
                                                                                    { expression = Expression'Case
                                                                                        ( CaseExpression
                                                                                            { base = Nothing
                                                                                            , cases =
                                                                                                ( Expression'GreaterThanOrEquals
                                                                                                    ( Expression'Multiply
                                                                                                        ( Expression'Multiply
                                                                                                            ( Expression'BitwiseNegate
                                                                                                                ( Expression'LiteralValue
                                                                                                                    ( Number "11" )
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
                                                                                                        ( Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "c"
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    )
                                                                                                    ( Expression'LiteralValue
                                                                                                        ( Number "17" )
                                                                                                    )
                                                                                                , Expression'Multiply
                                                                                                    ( Expression'FunctionCall
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
                                                                                                                                                { value = Expression'Divide
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
                                                                                                                                                                            ( Expression'Plus
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
                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                        , value = "c"
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
                                                                                                                                                                                                [ Expression'Minus
                                                                                                                                                                                                    ( Expression'Multiply
                                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                , value = "f"
                                                                                                                                                                                                                }
                                                                                                                                                                                                            )
                                                                                                                                                                                                        )
                                                                                                                                                                                                        ( Expression'Subquery
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
                                                                                                                                                                                                                                                    , value = "abs"
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                    [ Expression'FunctionCall
                                                                                                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                    , value = "min"
                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                                    [ Expression'Column
                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                            , value = "e"
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
                                                                                                                                                                                                                            , where_ = Nothing
                                                                                                                                                                                                                            , groupBy = Nothing
                                                                                                                                                                                                                            , window = Nothing
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    )
                                                                                                                                                                                                                , orderBy = Nothing
                                                                                                                                                                                                                , limit = Nothing
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
                                                                                                                                                                                                    ( Expression'Negate
                                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                , value = "a"
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
                                                                                                                                                                        )
                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                , value = "a"
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
                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            , value = "d"
                                                                                                                                                                            }
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
                                                                                                                                            ( Expression'And
                                                                                                                                                ( Expression'Not
                                                                                                                                                    ( Expression'InValues
                                                                                                                                                        ( InValuesExpression
                                                                                                                                                            { expression = Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "f"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            , values =
                                                                                                                                                                [ Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "e"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                , Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "b"
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
                                                                                                                                                                        , value = "f"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                ]
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                                ( Expression'NotEquals
                                                                                                                                                    ( Expression'Column
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "c"
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
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "f"
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
                                                                                                        ( Number "19" )
                                                                                                    )
                                                                                                ) :| []
                                                                                            , else_ = Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "f"
                                                                                                    }
                                                                                                )
                                                                                            }
                                                                                        )
                                                                                    , values =
                                                                                        [ Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "b"
                                                                                                }
                                                                                            )
                                                                                        , Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "e"
                                                                                                }
                                                                                            )
                                                                                        , Expression'LiteralValue
                                                                                            ( Number "17" )
                                                                                        ]
                                                                                    }
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
                                                    , Expression'LiteralValue
                                                        ( Number "13" )
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
                            ( Expression'Not
                                ( Expression'GreaterThanOrEquals
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "f"
                                            }
                                        )
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