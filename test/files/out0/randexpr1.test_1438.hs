Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Case
                                ( CaseExpression
                                    { base = Just
                                        ( Expression'Multiply
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "e"
                                                    }
                                                )
                                            )
                                            ( Expression'Case
                                                ( CaseExpression
                                                    { base = Nothing
                                                    , cases =
                                                        ( Expression'Or
                                                            ( Expression'InValues
                                                                ( InValuesExpression
                                                                    { expression = Expression'Plus
                                                                        ( Expression'Case
                                                                            ( CaseExpression
                                                                                { base = Just
                                                                                    ( Expression'Minus
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
                                                                                                                                                        ( Expression'InValues
                                                                                                                                                            ( InValuesExpression
                                                                                                                                                                { expression = Expression'Column
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
                                                                                                                                                                , values =
                                                                                                                                                                    [ Expression'LiteralValue
                                                                                                                                                                        ( Number "17" )
                                                                                                                                                                    , Expression'Column
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "b"
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
                                                                                                                , value = "e"
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
                                                                                                , value = "c"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                , cases =
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
                                                                                            , value = "c"
                                                                                            }
                                                                                        )
                                                                                    , Expression'LiteralValue
                                                                                        ( Number "19" )
                                                                                    ) :| []
                                                                                , else_ = Expression'Negate
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
                                                                                }
                                                                            )
                                                                        )
                                                                        ( Expression'LiteralValue
                                                                            ( Number "17" )
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
                                                                                , value = "d"
                                                                                }
                                                                            )
                                                                        , Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "c"
                                                                                }
                                                                            )
                                                                        , Expression'LiteralValue
                                                                            ( Number "11" )
                                                                        ]
                                                                    }
                                                                )
                                                            )
                                                            ( Expression'LessThanOrEquals
                                                                ( Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "b"
                                                                        }
                                                                    )
                                                                )
                                                                ( Expression'Negate
                                                                    ( Expression'Negate
                                                                        ( Expression'Negate
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "a"
                                                                                    }
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        , Expression'BitwiseNegate
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "a"
                                                                    }
                                                                )
                                                            )
                                                        ) :|
                                                        [
                                                            ( Expression'InSubquery
                                                                ( InSubqueryExpression
                                                                    { expression = Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "f"
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
                                                                                                { value = Expression'Case
                                                                                                    ( CaseExpression
                                                                                                        { base = Just
                                                                                                            ( Expression'Minus
                                                                                                                ( Expression'Plus
                                                                                                                    ( Expression'Case
                                                                                                                        ( CaseExpression
                                                                                                                            { base = Just
                                                                                                                                ( Expression'FunctionCall
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
                                                                                                                                )
                                                                                                                            , cases =
                                                                                                                                ( Expression'FunctionCall
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
                                                                                                                                , Expression'FunctionCall
                                                                                                                                    ( FunctionCallExpression
                                                                                                                                        { call = FunctionCall
                                                                                                                                            { name = Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "max"
                                                                                                                                                }
                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                [ Expression'LiteralValue
                                                                                                                                                    ( Number "17" )
                                                                                                                                                ]
                                                                                                                                            }
                                                                                                                                        , filter = Nothing
                                                                                                                                        , over = Nothing
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                ) :| []
                                                                                                                            , else_ = Expression'FunctionCall
                                                                                                                                ( FunctionCallExpression
                                                                                                                                    { call = FunctionCall
                                                                                                                                        { name = Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "max"
                                                                                                                                            }
                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                            [ Expression'LiteralValue
                                                                                                                                                ( Number "17" )
                                                                                                                                            ]
                                                                                                                                        }
                                                                                                                                    , filter = Nothing
                                                                                                                                    , over = Nothing
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                    ( Expression'AggregateDistinctFunctionCall
                                                                                                                        ( AggregateDistinctFunctionCallExpression
                                                                                                                            { call = FunctionCall
                                                                                                                                { name = Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "count"
                                                                                                                                    }
                                                                                                                                , arguments = Identity
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "e"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                }
                                                                                                                            , filter = Nothing
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                )
                                                                                                                ( Expression'Negate
                                                                                                                    ( Expression'AggregateDistinctFunctionCall
                                                                                                                        ( AggregateDistinctFunctionCallExpression
                                                                                                                            { call = FunctionCall
                                                                                                                                { name = Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "count"
                                                                                                                                    }
                                                                                                                                , arguments = Identity
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "d"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                }
                                                                                                                            , filter = Nothing
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                )
                                                                                                            )
                                                                                                        , cases =
                                                                                                            ( Expression'Negate
                                                                                                                ( Expression'FunctionCall
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
                                                                                                                )
                                                                                                            , Expression'Negate
                                                                                                                ( Expression'AggregateDistinctFunctionCall
                                                                                                                    ( AggregateDistinctFunctionCallExpression
                                                                                                                        { call = FunctionCall
                                                                                                                            { name = Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "count"
                                                                                                                                }
                                                                                                                            , arguments = Identity
                                                                                                                                ( Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "d"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            }
                                                                                                                        , filter = Nothing
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            ) :| []
                                                                                                        , else_ = Expression'FunctionCall
                                                                                                            ( FunctionCallExpression
                                                                                                                { call = FunctionCall
                                                                                                                    { name = Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "min"
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
                                                                                                                                , value = "f"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        ]
                                                                                                                    }
                                                                                                                , filter = Nothing
                                                                                                                , over = Nothing
                                                                                                                }
                                                                                                            )
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
                                                                            )
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
                                                                                                            , value = "min"
                                                                                                            }
                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                            [ Expression'Negate
                                                                                                                ( Expression'LiteralValue
                                                                                                                    ( Number "19" )
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
                                                                    }
                                                                )
                                                            , Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "c"
                                                                    }
                                                                )
                                                            )
                                                        ]
                                                    , else_ = Expression'LiteralValue
                                                        ( Number "13" )
                                                    }
                                                )
                                            )
                                        )
                                    , cases =
                                        ( Expression'Column
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
                                                , value = "b"
                                                }
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
                        ( Expression'LessThan
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
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )