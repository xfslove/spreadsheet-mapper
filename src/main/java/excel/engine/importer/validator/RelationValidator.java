package excel.engine.importer.validator;

import excel.engine.model.excel.ExcelMeta;

import java.util.Set;

/**
 * relation validator, after workbook and sheet validators, if post validators failure, relation validators will skip.
 * <p>
 * Created by hanwen on 2016/12/26.
 */
public interface RelationValidator<META extends ExcelMeta> extends Validator<META> {

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
