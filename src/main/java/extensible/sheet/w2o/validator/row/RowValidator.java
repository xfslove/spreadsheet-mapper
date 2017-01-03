package extensible.sheet.w2o.validator.row;


import extensible.sheet.model.core.Row;
import extensible.sheet.model.meta.SheetMeta;
import extensible.sheet.w2o.validator.RelationValidator;

import java.util.Set;

/**
 * row values validator, after sheet validators.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface RowValidator extends RelationValidator {

  /**
   * valid supplied excel row
   *
   * @param row       row
   * @param sheetMeta sheet meta
   * @return true if pass
   */
  boolean valid(Row row, SheetMeta sheetMeta);

  /**
   * @return error message on which fields
   */
  Set<String> getMessageOnFields();
}
