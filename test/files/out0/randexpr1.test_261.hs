Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Multiply
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "f"
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
                                                                            { value = Expression'FunctionCall
                                                                                ( FunctionCallExpression
                                                                                    { call = FunctionCall
                                                                                        { name = Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "max"
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
                                                                                                                                        , value = "count"
                                                                                                                                        }
                                                                                                                                    , arguments = FunctionArguments'Wildcard
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
                                                                        ( Expression'GreaterThanOrEquals
                                                                            ( Expression'Plus
                                                                                ( Expression'Multiply
                                                                                    ( Expression'BitwiseNegate
                                                                                        ( Expression'Case
                                                                                            ( CaseExpression
                                                                                                { base = Nothing
                                                                                                , cases =
                                                                                                    ( Expression'Or
                                                                                                        ( Expression'LessThanOrEquals
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
                                                                                                            ( Expression'Minus
                                                                                                                ( Expression'Case
                                                                                                                    ( CaseExpression
                                                                                                                        { base = Nothing
                                                                                                                        , cases =
                                                                                                                            ( Expression'GreaterThanOrEquals
                                                                                                                                ( Expression'LiteralValue
                                                                                                                                    ( Number "17" )
                                                                                                                                )
                                                                                                                                ( Expression'Case
                                                                                                                                    ( CaseExpression
                                                                                                                                        { base = Nothing
                                                                                                                                        , cases =
                                                                                                                                            ( Expression'InValues
                                                                                                                                                ( InValuesExpression
                                                                                                                                                    { expression = Expression'Multiply
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
                                                                                                                                                                , value = "a"
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                    , values =
                                                                                                                                                        [ Expression'Column
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Nothing
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
                                                                                                                                                        , Expression'LiteralValue
                                                                                                                                                            ( Number "13" )
                                                                                                                                                        ]
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
                                                                                                                                                    , value = "e"
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            ) :|
                                                                                                                                            [
                                                                                                                                                ( Expression'And
                                                                                                                                                    ( Expression'GreaterThan
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
                                                                                                                                                    )
                                                                                                                                                    ( Expression'LessThanOrEquals
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
                                                                                                                                                , Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "c"
                                                                                                                                                        }
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
                                                                                                                                                , value = "e"
                                                                                                                                                }
                                                                                                                                            )
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
                                                                                                                                    , value = "c"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            ) :| []
                                                                                                                        , else_ = Expression'LiteralValue
                                                                                                                            ( Number "17" )
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                                ( Expression'Multiply
                                                                                                                    ( Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "f"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                    ( Expression'LiteralValue
                                                                                                                        ( Number "17" )
                                                                                                                    )
                                                                                                                )
                                                                                                            )
                                                                                                        )
                                                                                                        ( Expression'Between
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
                                                                                                                    , value = "b"
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                            ( Expression'LiteralValue
                                                                                                                ( Number "19" )
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
                                                                                                            , value = "c"
                                                                                                            }
                                                                                                        )
                                                                                                    ) :|
                                                                                                    [
                                                                                                        ( Expression'LessThanOrEquals
                                                                                                            ( Expression'Negate
                                                                                                                ( Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "f"
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            )
                                                                                                            ( Expression'LiteralValue
                                                                                                                ( Number "11" )
                                                                                                            )
                                                                                                        , Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "c"
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    ]
                                                                                                , else_ = Expression'Negate
                                                                                                    ( Expression'LiteralValue
                                                                                                        ( Number "11" )
                                                                                                    )
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
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "d"
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
                                                    ( Number "17" )
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
                                ( Expression'Between
                                    ( Expression'LiteralValue
                                        ( Number "19" )
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
                                    ( Expression'BitwiseNegate
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "e"
                                                }
                                            )
                                        )
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