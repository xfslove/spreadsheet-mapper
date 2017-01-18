package spreadsheet.mapper.utils.builder;

import spreadsheet.mapper.model.meta.SheetMeta;

/**
 * sheet meta builder
 * <p>
 * Created by hanwen on 2017/1/18.
 */
public interface SheetMetaBuilder {

  /**
   * @return {@link SheetMeta}
   */
  SheetMeta toSheetMeta();
}
