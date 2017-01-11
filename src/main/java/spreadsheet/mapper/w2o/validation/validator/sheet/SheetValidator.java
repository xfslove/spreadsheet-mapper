package spreadsheet.mapper.w2o.validation.validator.sheet;

import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.w2o.validation.validator.Validator;

/**
 * sheet validator, after workbook validators, if workbook validators failure, sheet validators will skip.
 * <p>
 * Created by hanwen on 2016/12/23.
 */
public interface SheetValidator extends Validator {

  /**
   * valid supplied excel sheet
   *
   * @param sheet     {@link Sheet}
   * @param sheetMeta {@link SheetMeta}
   * @return true if pass
   */
  boolean valid(Sheet sheet, SheetMeta sheetMeta);
}
