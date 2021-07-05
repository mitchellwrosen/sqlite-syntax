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
                                    ( Expression'Plus
                                        ( Expression'Case
                                            ( CaseExpression
                                                { base = Nothing
                                                , cases =
                                                    ( Expression'NotEquals
                                                        ( Expression'Case
                                                            ( CaseExpression
                                                                { base = Nothing
                                                                , cases =
                                                                    ( Expression'Or
                                                                        ( Expression'LessThan
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
                                                                            ( Expression'BitwiseNegate
                                                                                ( Expression'LiteralValue
                                                                                    ( Number "17" )
                                                                                )
                                                                            )
                                                                        )
                                                                        ( Expression'NotEquals
                                                                            ( Expression'Multiply
                                                                                ( Expression'Case
                                                                                    ( CaseExpression
                                                                                        { base = Just
                                                                                            ( Expression'Case
                                                                                                ( CaseExpression
                                                                                                    { base = Just
                                                                                                        ( Expression'Case
                                                                                                            ( CaseExpression
                                                                                                                { base = Just
                                                                                                                    ( Expression'Multiply
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
                                                                                                                            ( Expression'LiteralValue
                                                                                                                                ( Number "17" )
                                                                                                                            )
                                                                                                                        )
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "13" )
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
                                                                                                                            , value = "a"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    , Expression'Plus
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
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
                                                                                                    , cases =
                                                                                                        ( Expression'Plus
                                                                                                            ( Expression'Multiply
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
                                                                                                                        , value = "f"
                                                                                                                        }
                                                                                                                    )
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
                                                                                                                                                , where_ = Just
                                                                                                                                                    ( Expression'Not
                                                                                                                                                        ( Expression'Between
                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                ( Number "13" )
                                                                                                                                                            )
                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                ( Number "13" )
                                                                                                                                                            )
                                                                                                                                                            ( Expression'Negate
                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                    ( Number "13" )
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
                                                                                                                                ( Number "17" )
                                                                                                                            ]
                                                                                                                        }
                                                                                                                    , filter = Nothing
                                                                                                                    , over = Nothing
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
                                                                                        , cases =
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
                                                                                            , Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "a"
                                                                                                    }
                                                                                                )
                                                                                            ) :| []
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
                                                                                    , value = "d"
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
                                                                                , value = "c"
                                                                                }
                                                                            )
                                                                        )
                                                                    ) :| []
                                                                , else_ = Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "a"
                                                                        }
                                                                    )
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
                                                    , Expression'Negate
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
                                                    ) :| []
                                                , else_ = Expression'Column
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
                                            , value = "c"
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
                            ( Expression'GreaterThan
                                ( Expression'Case
                                    ( CaseExpression
                                        { base = Nothing
                                        , cases =
                                            ( Expression'InValues
                                                ( InValuesExpression
                                                    { expression = Expression'Minus
                                                        ( Expression'BitwiseNegate
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "f"
                                                                    }
                                                                )
                                                            )
                                                        )
                                                        ( Expression'Negate
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "e"
                                                                    }
                                                                )
                                                            )
                                                        )
                                                    , values =
                                                        [ Expression'LiteralValue
                                                            ( Number "17" )
                                                        , Expression'Case
                                                            ( CaseExpression
                                                                { base = Nothing
                                                                , cases =
                                                                    ( Expression'InSubquery
                                                                        ( InSubqueryExpression
                                                                            { expression = Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "d"
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
                                                                                                        { value = Expression'Column
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
                                                                                                    { value = Expression'Plus
                                                                                                        ( Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "a"
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                        ( Expression'Multiply
                                                                                                            ( Expression'Multiply
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
                                                                                                                                                                            , where_ = Just
                                                                                                                                                                                ( Expression'InSubquery
                                                                                                                                                                                    ( InSubqueryExpression
                                                                                                                                                                                        { expression = Expression'Column
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "a"
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
                                                                                                                                                                                                                    { value = Expression'BitwiseOr
                                                                                                                                                                                                                        ( Expression'FunctionCall
                                                                                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                        , value = "min"
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
                                                                                                                                                                                                                        )
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
                                                                                                                )
                                                                                                                ( Expression'LiteralValue
                                                                                                                    ( Number "19" )
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
                                                                            , value = "b"
                                                                            }
                                                                        )
                                                                    ) :|
                                                                    [
                                                                        ( Expression'Not
                                                                            ( Expression'Between
                                                                                ( Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "a"
                                                                                        }
                                                                                    )
                                                                                )
                                                                                ( Expression'LiteralValue
                                                                                    ( Number "13" )
                                                                                )
                                                                                ( Expression'LiteralValue
                                                                                    ( Number "13" )
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
                                                                                , value = "f"
                                                                                }
                                                                            )
                                                                        )
                                                                    ]
                                                                , else_ = Expression'LiteralValue
                                                                    ( Number "17" )
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
                                            , Expression'Subquery
                                                ( SelectStatement
                                                    { commonTableExpressions = Nothing
                                                    , select = CompoundSelect
                                                        ( SelectCore'Select
                                                            ( Select
                                                                { distinct = False
                                                                , columns = ResultColumn'Expression
                                                                    ( Aliased
                                                                        { value = Expression'BitwiseOr
                                                                            ( Expression'Minus
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