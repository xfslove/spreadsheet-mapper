package spreadsheet.mapper.w2o.validation.validator.workbook;


import spreadsheet.mapper.model.core.Workbook;
import spreadsheet.mapper.model.meta.WorkbookMeta;
import spreadsheet.mapper.w2o.validation.validator.Validator;

/**
 * workbook template validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface WorkbookValidator extends Validator {

  /**
   * valid supplied workbook
   *
   * @param workbook     {@link Workbook}
   * @param workbookMeta {@link WorkbookMeta}
   * @return true if pass
   */
  boolean valid(Workbook workbook, WorkbookMeta workbookMeta);

  /**
   * @return message on which sheet
   */
  int getMessageOnSheet();
}
