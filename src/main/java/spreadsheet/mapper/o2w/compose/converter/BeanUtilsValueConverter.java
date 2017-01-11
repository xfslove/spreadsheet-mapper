package spreadsheet.mapper.o2w.compose.converter;

import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.o2w.compose.WorkbookComposeException;
import spreadsheet.mapper.utils.FieldUtils;


/**
 * using {@link BeanUtils#getProperty(Object, String)}, this use to fallback
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class BeanUtilsValueConverter<T> implements ValueConverter<T> {

  private static final Logger LOGGER = LoggerFactory.getLogger(BeanUtilsValueConverter.class);

  @Override
  public String getStringValue(T object, FieldMeta fieldMeta) {
    try {
      return BeanUtils.getProperty(object, FieldUtils.detectRealFieldName(fieldMeta));
    } catch (NestedNullException e) {
      LOGGER.debug("{} is null", fieldMeta.getName());
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookComposeException(e);
    }
  }

}
