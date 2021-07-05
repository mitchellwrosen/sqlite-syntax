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
                                                                        { value = Expression'Case
                                                                            ( CaseExpression
                                                                                { base = Just
                                                                                    ( Expression'Minus
                                                                                        ( Expression'Case
                                                                                            ( CaseExpression
                                                                                                { base = Nothing
                                                                                                , cases =
                                                                                                    ( Expression'GreaterThan
                                                                                                        ( Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "f"
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
                                                                                                                , value = "d"
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    , Expression'Minus
                                                                                                        ( Expression'Plus
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
                                                                                                        ( Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "f"
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    ) :|
                                                                                                    [
                                                                                                        ( Expression'Not
                                                                                                            ( Expression'InValues
                                                                                                                ( InValuesExpression
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
                                                                                                                                                                                    ( Expression'Multiply
                                                                                                                                                                                        ( Expression'Case
                                                                                                                                                                                            ( CaseExpression
                                                                                                                                                                                                { base = Just
                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                            , value = "f"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
                                                                                                                                                                                                    )
                                                                                                                                                                                                , cases =
                                                                                                                                                                                                    ( Expression'Multiply
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
                                                                                                                                                                                                    , Expression'LiteralValue
                                                                                                                                                                                                        ( Number "11" )
                                                                                                                                                                                                    ) :| []
                                                                                                                                                                                                , else_ = Expression'LiteralValue
                                                                                                                                                                                                    ( Number "11" )
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
                                                                                                                                                                                                { value = Expression'FunctionCall
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
                                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                    , value = "f"
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                )
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
                                                                                                                                , value = "a"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        ]
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        , Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "f"
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
                                                                                                        , value = "b"
                                                                                                        }
                                                                                                    )
                                                                                                }
                                                                                            )
                                                                                        )
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
                                                                                                    , value = "a"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                , cases =
                                                                                    ( Expression'Negate
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "11" )
                                                                                        )
                                                                                    , Expression'LiteralValue
                                                                                        ( Number "19" )
                                                                                    ) :| []
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
                                                                        ( Expression'Between
                                                                            ( Expression'LiteralValue
                                                                                ( Number "17" )
                                                                            )
                                                                            ( Expression'Negate
                                                                                ( Expression'LiteralValue
                                                                                    ( Number "13" )
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
                                            , Expression'LiteralValue
                                                ( Number "19" )
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
                                ( Expression'LessThan
                                    ( Expression'Multiply
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
                                                                                                    [ Expression'BitwiseOr
                                                                                                        ( Expression'LiteralValue
                                                                                                            ( Number "17" )
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
                                                                                                                                                                                                                                                                ( Expression'Case
                                                                                                                                                                                                                                                                    ( CaseExpression
                                                                                                                                                                                                                                                                        { base = Nothing
                                                                                                                                                                                                                                                                        , cases =
                                                                                                                                                                                                                                                                            ( Expression'And
                                                                                                                                                                                                                                                                                ( Expression'Not
                                                                                                                                                                                                                                                                                    ( Expression'InValues
                                                                                                                                                                                                                                                                                        ( InValuesExpression
                                                                                                                                                                                                                                                                                            { expression = Expression'Column
                                                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                                                    , value = "b"
                                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                                            , values =
                                                                                                                                                                                                                                                                                                [ Expression'Column
                                                                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                                                                        , value = "a"
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
                                                                                                                                                                                                                                                                                                , Expression'Negate
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
                                                                                                                                                                                                                                                                                ( Expression'Equals
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
                                                                                                                                                                                                                                                                            , Expression'Minus
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
                                                                                                                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                            ) :|
                                                                                                                                                                                                                                                                            [
                                                                                                                                                                                                                                                                                ( Expression'GreaterThan
                                                                                                                                                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                                                                                                                                                        ( Number "11" )
                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                                            , value = "d"
                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                , Expression'LiteralValue
                                                                                                                                                                                                                                                                                    ( Number "17" )
                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                                                                        , else_ = Expression'LiteralValue
                                                                                                                                                                                                                                                                            ( Number "11" )
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
                                                                                                                                                                                                                                        ( Expression'NotEquals
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
                                                                                                                                                                                                                , Expression'Negate
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
                                                                                                                                                                                                                                                , value = "min"
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                                [ Expression'Column
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
                                                                                                                                                                                                                            { value = Expression'BitwiseOr
                                                                                                                                                                                                                                ( Expression'Plus
                                                                                                                                                                                                                                    ( Expression'Plus
                                                                                                                                                                                                                                        ( Expression'FunctionCall
                                                                                                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                        , value = "max"
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
                                                                                                                                                                                                                                ( Expression'Multiply
                                                                                                                                                                                                                                    ( Expression'Negate
                                                                                                                                                                                                                                        ( Expression'Negate
                                                                                                                                                                                                                                            ( Expression'AggregateDistinctFunctionCall
                                                                                                                                                                                                                                                ( AggregateDistinctFunctionCallExpression
                                                                                                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                            , value = "count"
                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                        , arguments = Identity
                                                                                                                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                                                                                                                ( Number "17" )
                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                    )
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