Statement'Select
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
                                                                                        [ Expression'Multiply
                                                                                            ( Expression'Case
                                                                                                ( CaseExpression
                                                                                                    { base = Just
                                                                                                        ( Expression'LiteralValue
                                                                                                            ( Number "13" )
                                                                                                        )
                                                                                                    , cases =
                                                                                                        ( Expression'Plus
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
                                                                                                                                                        { value = Expression'FunctionCall
                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "max"
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
                                                                                                                                                            { expression = Expression'Plus
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
                                                                                                                                                                ( Expression'Column
                                                                                                                                                                    ( Namespaced
                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                        , value = "f"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                            , values =
                                                                                                                                                                [ Expression'Column
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
                                                                                                                                                                , Expression'Minus
                                                                                                                                                                    ( Expression'Divide
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
                                                                                                                                                                        ( Expression'FunctionCall
                                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                        , value = "abs"
                                                                                                                                                                                        }
                                                                                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                                                                                        [ Expression'Plus
                                                                                                                                                                                            ( Expression'Plus
                                                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                                                    ( Number "17" )
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
                                                                                                                                                                                                                        [ Expression'Plus
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
                                                                                                                                                                                                                        [ Expression'BitwiseNegate
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
                                                                                                                                                                                            ( Expression'Case
                                                                                                                                                                                                ( CaseExpression
                                                                                                                                                                                                    { base = Nothing
                                                                                                                                                                                                    , cases =
                                                                                                                                                                                                        ( Expression'Not
                                                                                                                                                                                                            ( Expression'Exists
                                                                                                                                                                                                                ( SelectStatement
                                                                                                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                                                                                                    , select = CompoundSelect
                                                                                                                                                                                                                        ( SelectCore'Select
                                                                                                                                                                                                                            ( Select
                                                                                                                                                                                                                                { distinct = False
                                                                                                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                                                                                                    ( Aliased
                                                                                                                                                                                                                                        { value = Expression'LiteralValue
                                                                                                                                                                                                                                            ( Number "1" )
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
                                                                                                                                                                                                                                    ( Expression'Equals
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
                                                                                                                                                                                                                                                                                    { value = Expression'FunctionCall
                                                                                                                                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                                                    , value = "max"
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
                                                                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                                            , value = "b"
                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                    ( Expression'Negate
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
                                                                                                                                                                                                        , Expression'LiteralValue
                                                                                                                                                                                                            ( Number "19" )
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
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "f"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    )
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
                                                                                                            )
                                                                                                            ( Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "a"
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        , Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "f"
                                                                                                                }
                                                                                                            )
                                                                                                        ) :| []
                                                                                                    , else_ = Expression'LiteralValue
                                                                                                        ( Number "17" )
                                                                                                    }
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
                                                                    ( Expression'NotEquals
                                                                        ( Expression'LiteralValue
                                                                            ( Number "13" )
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
                                                    , value = "f"
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
                    , where_ = Just
                        ( Expression'Not
                            ( Expression'Not
                                ( Expression'Exists
                                    ( SelectStatement
                                        { commonTableExpressions = Nothing
                                        , select = CompoundSelect
                                            ( SelectCore'Select
                                                ( Select
                                                    { distinct = False
                                                    , columns = ResultColumn'Expression
                                                        ( Aliased
                                                            { value = Expression'LiteralValue
                                                                ( Number "1" )
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
                                                        ( Expression'Or
                                                            ( Expression'InSubquery
                                                                ( InSubqueryExpression
                                                                    { expression = Expression'FunctionCall
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
                                                                                                                                [ Expression'BitwiseOr
                                                                                                                                    ( Expression'BitwiseOr
                                                                                                                                        ( Expression'Minus
                                                                                                                                            ( Expression'Plus
                                                                                                                                                ( Expression'Plus
                                                                                                                                                    ( Expression'Case
                                                                                                                                                        ( CaseExpression
                                                                                                                                                            { base = Nothing
                                                                                                                                                            , cases =
                                                                                                                                                                ( Expression'Not
                                                                                                                                                                    ( Expression'InValues
                                                                                                                                                                        ( InValuesExpression
                                                                                                                                                                            { expression = Expression'LiteralValue
                                                                                                                                                                                ( Number "13" )
                                                                                                                                                                            , values =
                                                                                                                                                                                [ Expression'Column
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
                                                                                                                                                                                        , value = "c"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                ]
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    )
                                                                                                                                                                , Expression'BitwiseOr
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
                                                                                                                                                                ) :| []
                                                                                                                                                            , else_ = Expression'Minus
                                                                                                                                                                ( Expression'Plus
                                                                                                                                                                    ( Expression'Plus
                                                                                                                                                                        ( Expression'Minus
                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                ( Number "13" )
                                                                                                                                                                            )
                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                ( Number "19" )
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
                                                                                                                                                                        , value = "e"
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                )
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                    ( Expression'Multiply
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
                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                            ( Number "17" )
                                                                                                                                                        )
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
                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                ( Number "17" )
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                        ( Expression'Minus
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
                                                                                                                                    ( Expression'Plus
                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                            ( Number "19" )
                                                                                                                                        )
                                                                                                                                        ( Expression'Column
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "b"
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
                                                                                                                ( Expression'Equals
                                                                                                                    ( Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "a"
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
                                                                                        ( Number "11" )
                                                                                    ]
                                                                                }
                                                                            , filter = Nothing
                                                                            , over = Nothing
                                                                            }
                                                                        )
                                                                    , subquery = SelectStatement
                                                                        { commonTableExpressions = Nothing
                                                                        , select = Union
                                                                            ( CompoundSelect
                                                                                ( SelectCore'Select
                                                                                    ( Select
                                                                                        { distinct = False
                                                                                        , columns = ResultColumn'Expression
                                                                                            ( Aliased
                                                                                                { value = Expression'LiteralValue
                                                                                                    ( Number "17" )
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
                                                                            )
                                                                            ( SelectCore'Select
                                                                                ( Select
                                                                                    { distinct = False
                                                                                    , columns = ResultColumn'Expression
                                                                                        ( Aliased
                                                                                            { value = Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "c"
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
                                                                    }
                                                                )
                                                            )
                                                            ( Expression'GreaterThanOrEquals
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