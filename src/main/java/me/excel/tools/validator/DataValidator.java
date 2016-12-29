package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelMeta;

import java.util.Set;

/**
 * data validator, after workbook and sheet validators, if post validators failure, data validators will skip.
 * <p>
 * Created by hanwen on 2016/12/26.
 */
public interface DataValidator<META extends ExcelMeta> extends Validator<META> {

  /**
   * the validator key to identify unique validator
   *
   * @return the key
   */
  String getKey();

  /**
   * the validator depends on which validators result, means if any one depends on validator failure, this validator will skip.
   *
   * @return depends on keys
   */
  Set<String> getDependsOn();
}
