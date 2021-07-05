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
                                                            { base = Just
                                                                ( Expression'BitwiseOr
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
                                                                            , value = "d"
                                                                            }
                                                                        )
                                                                    )
                                                                )
                                                            , cases =
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
                                                                                                                            [ Expression'BitwiseNegate
                                                                                                                                ( Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "b"
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
                                                                                                            ( Expression'Multiply
                                                                                                                ( Expression'Negate
                                                                                                                    ( Expression'Case
                                                                                                                        ( CaseExpression
                                                                                                                            { base = Just
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
                                                                                                                            , cases =
                                                                                                                                ( Expression'BitwiseOr
                                                                                                                                    ( Expression'BitwiseOr
                                                                                                                                        ( Expression'Plus
                                                                                                                                            ( Expression'Minus
                                                                                                                                                ( Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "b"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                                ( Expression'Case
                                                                                                                                                    ( CaseExpression
                                                                                                                                                        { base = Nothing
                                                                                                                                                        , cases =
                                                                                                                                                            ( Expression'InSubquery
                                                                                                                                                                ( InSubqueryExpression
                                                                                                                                                                    { expression = Expression'LiteralValue
                                                                                                                                                                        ( Number "17" )
                                                                                                                                                                    , subquery = SelectStatement
                                                                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                                                                        , select = Union
                                                                                                                                                                            ( CompoundSelect
                                                                                                                                                                                ( SelectCore'Select
                                                                                                                                                                                    ( Select
                                                                                                                                                                                        { distinct = False
                                                                                                                                                                                        , columns = ResultColumn'Expression
                                                                                                                                                                                            ( Aliased
                                                                                                                                                                                                { value = Expression'Plus
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
                                                                                                                                                                                                                            , value = "b"
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    )
                                                                                                                                                                                                                }
                                                                                                                                                                                                            , filter = Nothing
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
                                                                                                                                                                                                                }
                                                                                                                                                                                                            , filter = Nothing
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
                                                                                                                                                                                            { value = Expression'AggregateDistinctFunctionCall
                                                                                                                                                                                                ( AggregateDistinctFunctionCallExpression
                                                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                            , value = "count"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        , arguments = Identity
                                                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                                                ( Number "13" )
                                                                                                                                                                                                            )
                                                                                                                                                                                                        }
                                                                                                                                                                                                    , filter = Nothing
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
                                                                                                                                                                    , value = "d"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            ) :| []
                                                                                                                                                        , else_ = Expression'LiteralValue
                                                                                                                                                            ( Number "13" )
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
                                                                                                                                                    , value = "d"
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                        ( Expression'Plus
                                                                                                                                            ( Expression'Multiply
                                                                                                                                                ( Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "d"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                                ( Expression'Negate
                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                        ( Number "11" )
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
                                                                                                                                                        , value = "d"
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
                                                                                                                                                        , value = "f"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                    ( Expression'Minus
                                                                                                                                        ( Expression'Column
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "a"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                            ( Number "17" )
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                , Expression'LiteralValue
                                                                                                                                    ( Number "13" )
                                                                                                                                ) :| []
                                                                                                                            , else_ = Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "c"
                                                                                                                                    }
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
                                                                                , Expression'Column
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
                                                                    , value = "d"
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
                                                    [ Expression'Column
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
                            ( Expression'InSubquery
                                ( InSubqueryExpression
                                    { expression = Expression'Column
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
                                    , subquery = SelectStatement
                                        { commonTableExpressions = Nothing
                                        , select = Union
                                            ( CompoundSelect
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