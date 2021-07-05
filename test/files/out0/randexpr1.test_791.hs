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
                                    , cases =
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "c"
                                                }
                                            )
                                        , Expression'Plus
                                            ( Expression'Minus
                                                ( Expression'Multiply
                                                    ( Expression'Negate
                                                        ( Expression'BitwiseNegate
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
                                                                                                            [ Expression'Divide
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
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "d"
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
                                                                                                                                [ Expression'Case
                                                                                                                                    ( CaseExpression
                                                                                                                                        { base = Nothing
                                                                                                                                        , cases =
                                                                                                                                            ( Expression'Or
                                                                                                                                                ( Expression'InSubquery
                                                                                                                                                    ( InSubqueryExpression
                                                                                                                                                        { expression = Expression'Minus
                                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                                ( Number "11" )
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
                                                                                                                                                                                                                        ( Expression'And
                                                                                                                                                                                                                            ( Expression'LessThanOrEquals
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
                                                                                                                                                                                                                                        , value = "d"
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                            ( Expression'Between
                                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                        , value = "e"
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                                                                                    ( Number "11" )
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
                                                                                                                                                ( Expression'InSubquery
                                                                                                                                                    ( InSubqueryExpression
                                                                                                                                                        { expression = Expression'LiteralValue
                                                                                                                                                            ( Number "11" )
                                                                                                                                                        , subquery = SelectStatement
                                                                                                                                                            { commonTableExpressions = Nothing
                                                                                                                                                            , select = Union
                                                                                                                                                                ( CompoundSelect
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
                                                                                                                                            , Expression'Column
                                                                                                                                                ( Namespaced
                                                                                                                                                    { namespace = Nothing
                                                                                                                                                    , value = "f"
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            ) :|
                                                                                                                                            [
                                                                                                                                                ( Expression'Equals
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
                                                                                                                                                            , value = "a"
                                                                                                                                                            }
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
                                                                                            ( Expression'Between
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
                                        ) :| []
                                    , else_ = Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
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
                                                        ( Expression'LessThanOrEquals
                                                            ( Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
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