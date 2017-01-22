package spreadsheet.mapper.w2o.validation;

import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.model.msg.Message;
import spreadsheet.mapper.model.msg.MessageWriteStrategies;
import spreadsheet.mapper.w2o.validation.validator.cell.DependencyValidator;
import spreadsheet.mapper.w2o.validation.validator.cell.SingleCellValidator;
import spreadsheet.mapper.w2o.validation.validator.row.RowValidator;
import spreadsheet.mapper.w2o.validation.validator.sheet.SheetValidator;

import java.util.List;

/**
 * sheet validation helper
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface SheetValidationHelper {

  /**
   * @param sheetValidator {@link SheetValidator}
   * @return {@link SheetValidationHelper}
   */
  SheetValidationHelper addSheetValidator(SheetValidator sheetValidator);

  /**
   * @param rowValidator {@link RowValidator}
   * @return {@link SheetValidationHelper}
   */
  SheetValidationHelper addRowValidator(RowValidator rowValidator);

  /**
   * @param dependencyValidator {@link DependencyValidator}
   * @return {@link SheetValidationHelper}
   */
  SheetValidationHelper addDependencyValidator(DependencyValidator dependencyValidator);

  /**
   * execute valid
   *
   * @param sheet     {@link Sheet}
   * @param sheetMeta {@link SheetMeta}
   * @return true if pass all
   */
  boolean valid(Sheet sheet, SheetMeta sheetMeta);

  /**
   * <pre>
   * message write strategy of {@link SheetValidator#getErrorMessage()} is {@link MessageWriteStrategies#TEXT_BOX}
   * message write strategy of {@link RowValidator#getErrorMessage()} &amp; {@link SingleCellValidator#getErrorMessage()} is {@link MessageWriteStrategies#COMMENT}
   * </pre>
   *
   * @return list of valid error messages
   */
  List<Message> getErrorMessages();
}
