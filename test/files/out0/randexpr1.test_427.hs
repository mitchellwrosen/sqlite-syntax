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
                                                [ Expression'Minus
                                                    ( Expression'Minus
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "e"
                                                                }
                                                            )
                                                        )
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
                                                                                                            ( Expression'Minus
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
                                                                                                                ( Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "a"
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            )
                                                                                                            ( Expression'Case
                                                                                                                ( CaseExpression
                                                                                                                    { base = Just
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
                                                                                                                                                                                            , value = "d"
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
                                                                                                                                                                ( Expression'NotEquals
                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                        ( Number "13" )
                                                                                                                                                                    )
                                                                                                                                                                    ( Expression'BitwiseNegate
                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                            ( Number "19" )
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
                                                                                                                    , cases =
                                                                                                                        ( Expression'BitwiseOr
                                                                                                                            ( Expression'Minus
                                                                                                                                ( Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "a"
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
                                                                                                                        , Expression'Minus
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
                                                                                                                                    ( Number "11" )
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
                                                                                                                        ) :| []
                                                                                                                    , else_ = Expression'BitwiseOr
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
                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            , value = "b"
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
                                                                                                                                                                                                ( Expression'Equals
                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                        ( Number "17" )
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
                                                                                                                                                                                                ( Expression'GreaterThanOrEquals
                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                            { namespace = Nothing
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
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        )
                                                                                                        ( Expression'LiteralValue
                                                                                                            ( Number "19" )
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
                                                                    , value = "c"
                                                                    }
                                                                )
                                                            )
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
                                                                , value = "c"
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
                        ( Expression'Not
                            ( Expression'Not
                                ( Expression'InValues
                                    ( InValuesExpression
                                        { expression = Expression'Minus
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "a"
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
                                        , values =
                                            [ Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "a"
                                                    }
                                                )
                                            , Expression'BitwiseOr
                                                ( Expression'BitwiseOr
                                                    ( Expression'Minus
                                                        ( Expression'Minus
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "a"
                                                                    }
                                                                )
                                                            )
                                                            ( Expression'LiteralValue
                                                                ( Number "19" )
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
                                                    ( Expression'Minus
                                                        ( Expression'Column
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
                                                        )
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "d"
                                                                }
                                                            )
                                                        )
                                                    )
                                                )
                                                ( Expression'Multiply
                                                    ( Expression'Multiply
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "e"
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
                                                                , value = "c"
                                                                }
                                                            )
                                                        )
                                                    )
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "b"
                                                            }
                                                        )
                                                    )
                                                )
                                            , Expression'Plus
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
                                            ]
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