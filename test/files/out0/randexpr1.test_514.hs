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
                                    { base = Nothing
                                    , cases =
                                        ( Expression'And
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
                                                                                                            [ Expression'Plus
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
                                                                                                                                , value = "a"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "17" )
                                                                                                                        )
                                                                                                                    )
                                                                                                                    ( Expression'LiteralValue
                                                                                                                        ( Number "11" )
                                                                                                                    )
                                                                                                                )
                                                                                                                ( Expression'Case
                                                                                                                    ( CaseExpression
                                                                                                                        { base = Nothing
                                                                                                                        , cases =
                                                                                                                            ( Expression'Or
                                                                                                                                ( Expression'InSubquery
                                                                                                                                    ( InSubqueryExpression
                                                                                                                                        { expression = Expression'Divide
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
                                                                                                                                        , subquery = SelectStatement
                                                                                                                                            { commonTableExpressions = Nothing
                                                                                                                                            , select = Union
                                                                                                                                                ( CompoundSelect
                                                                                                                                                    ( SelectCore'Select
                                                                                                                                                        ( Select
                                                                                                                                                            { distinct = False
                                                                                                                                                            , columns = ResultColumn'Expression
                                                                                                                                                                ( Aliased
                                                                                                                                                                    { value = Expression'BitwiseNegate
                                                                                                                                                                        ( Expression'Case
                                                                                                                                                                            ( CaseExpression
                                                                                                                                                                                { base = Just
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
                                                                                                                                                                                , cases =
                                                                                                                                                                                    ( Expression'FunctionCall
                                                                                                                                                                                        ( FunctionCallExpression
                                                                                                                                                                                            { call = FunctionCall
                                                                                                                                                                                                { name = Namespaced
                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                    , value = "min"
                                                                                                                                                                                                    }
                                                                                                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                    [ Expression'Negate
                                                                                                                                                                                                        ( Expression'Column
                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                { namespace = Nothing
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
                                                                                                                                                                                    , Expression'AggregateDistinctFunctionCall
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
                                                                                                                                                                                    ) :| []
                                                                                                                                                                                , else_ = Expression'FunctionCall
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
                                                                                                                                                                { value = Expression'Negate
                                                                                                                                                                    ( Expression'FunctionCall
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
                                                                                                                                            , value = "a"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            , Expression'Case
                                                                                                                                ( CaseExpression
                                                                                                                                    { base = Just
                                                                                                                                        ( Expression'Column
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Nothing
                                                                                                                                                , value = "c"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    , cases =
                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                            ( Number "13" )
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
                                                                                                                                        ) :| []
                                                                                                                                    , else_ = Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "e"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            ) :|
                                                                                                                            [
                                                                                                                                ( Expression'LessThanOrEquals
                                                                                                                                    ( Expression'Negate
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
                                                                                                                                , value = "c"
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
                                                                                                ( Expression'Negate
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
                                                                        , value = "b"
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
                                                        , value = "f"
                                                        }
                                                    )
                                                )
                                            )
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
                                        , Expression'BitwiseNegate
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
                                        ) :|
                                        [
                                            ( Expression'Equals
                                                ( Expression'LiteralValue
                                                    ( Number "17" )
                                                )
                                                ( Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "e"
                                                        }
                                                    )
                                                )
                                            , Expression'LiteralValue
                                                ( Number "19" )
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
                            ( Expression'GreaterThanOrEquals
                                ( Expression'Case
                                    ( CaseExpression
                                        { base = Nothing
                                        , cases =
                                            ( Expression'InSubquery
                                                ( InSubqueryExpression
                                                    { expression = Expression'Negate
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
                                                                                    ( Number "13" )
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
                                                                                                                                                                                    { value = Expression'BitwiseNegate
                                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                                            ( Number "13" )
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
                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                        ( Number "11" )
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
                                                                                                                                                        , Expression'Multiply
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
                                                                                                                , where_ = Just
                                                                                                                    ( Expression'Not
                                                                                                                        ( Expression'GreaterThanOrEquals
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
                                                                                                                            ( Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "c"
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
                                            , Expression'Minus
                                                ( Expression'Plus
                                                    ( Expression'Negate
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
                                                ( Expression'Multiply
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
                                                                                                        { namespace = Just
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "t1"
                                                                                                                }
                                                                                                            )
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
                                                                                                                            ( Expression'Negate
                                                                                                                                ( Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "b"
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
                                                                                                                                        , value = "f"
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
                                                    )
                                                    ( Expression'LiteralValue
                                                        ( Number "17" )
                                                    )
                                                )
                                            ) :|
                                            [
                                                ( Expression'LessThanOrEquals
                                                    ( Expression'LiteralValue
                                                        ( Number "19" )
                                                    )
                                                    ( Expression'LiteralValue
                                                        ( Number "11" )
                                                    )
                                                , Expression'LiteralValue
                                                    ( Number "19" )
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
                                                , value = "d"
                                                }
                                            )
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