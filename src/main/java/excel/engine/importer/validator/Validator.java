package excel.engine.importer.validator;

import excel.engine.model.excel.ExcelMeta;

/**
 * excel meta validator
 * <p>
 * Created by hanwen on 2016/12/28.
 */
public interface Validator<META extends ExcelMeta> {

  /**
   * @return valid error message
   */
  String getErrorMessage();

  /**
   * valid the supplied {@link ExcelMeta}
   *
   * @param excelMeta excel meta
   * @return true if pass
   */
  boolean valid(META excelMeta);
}
