{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                                                                              }
{                           +++++++++++++++++++++                              }
{                           +       +           +                              }
{                           +       +           +                              }
{                           +    +++++++++      +                              }
{                           +            +      +                              }
{                           +            +      +                              }
{                           +++++++      +      +                              }
{                           +            +      +                              }
{                           +            +      +                              }
{                           +++++++++++++++++++++                              }
{                                 SubSeven Legacy                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/                                   }
{                   https://github.com/darkcodersc                             }
{                   License: Apache License 2.0                                }
{                                                                              }
{                                                                              }
{  Disclaimer:                                                                 }
{  -----------                                                                 }
{    We are doing our best to prepare the content of this app and/or code.     }
{    However, The author cannot warranty the expressions and suggestions       }
{    of the contents, as well as its accuracy. In addition, to the extent      }
{    permitted by the law, author shall not be responsible for any losses      }
{    and/or damages due to the usage of the information on our app and/or      }
{    code.                                                                     }
{                                                                              }
{    By using our app and/or code, you hereby consent to our disclaimer        }
{    and agree to its terms.                                                   }
{                                                                              }
{    Any links contained in our app may lead to external sites are provided    }
{    for convenience only.                                                     }
{    Any information or statements that appeared in these sites or app or      }
{    files are not sponsored, endorsed, or otherwise approved by the author.   }
{    For these external sites, the author cannot be held liable for the        }
{    availability of, or the content located on or through it.                 }
{    Plus, any losses or damages occurred from using these contents or the     }
{    internet generally.                                                       }
{                                                                              }
{                                                                              }
{                                                                              }
{    I dedicate this work to my daughter.                                      }
{                                                                              }
{******************************************************************************}

unit Sub7.Core.Bundle;

interface

uses WinAPI.Messages;

const
      {
        Exception Messages
      }
      ERR_MISSING_PARAMETER    = 'Missing "%s" parameter.';
      ERR_MISSING_DATA_NODE    = 'Data node is missing from command.';
      ERR_UNEXPECTED           = 'Unexpected error has occured.';
      ERR_NOCHANGES            = 'No changes to apply so far.';
      ERR_IOERROR              = 'Could not read data from stream or memory.';
      ERR_UNEXPECTED_CMD       = 'Unexpected command.';
      ERR_MISSING_INSTANCE     = 'Missing object instance.';
      ERR_FILEINFO             = 'Could not retrieve file information: "%s".';
      ERR_DESERIALIZATION      = 'Could not deserialize "%s".';
      ERR_FILE_EXISTS          = 'File "%s" already exists.';
      ERR_FILE_MISSING         = 'File "%s" is missing.';
      ERR_INVALID_CLASS        = 'Invalid class.';
      ERR_INVALID_DATA         = 'Unexpected data or data is invalid.';
      ERR_TIMEOUT              = 'Action took to much time to respond.';
      ERR_FORM                 = 'Please complete all required form fields.';
      ERR_INVALID_FILE         = 'File is invalid or broken.';
      ERR_FILE_IO              = 'Could not open "%s" file.';
      ERR_PE                   = '"%s" is not a valid portable executable file.';
      ERR_GUID                 = 'Missing or invalid GUID.';
      ERR_MISSING_WINAPI       = '"%s" Windows API is missing.';
      ERR_INVALID_INSTANCE     = 'Invalid object instance/nature.';
      ERR_INVALID_SESSION      = 'Could not retrieve session object. Missing session or session token is not valid.';
      ERR_SESSION              = 'Could not attach worker to session.';
      ERR_RESOLVE_OBJECT       = 'Could not resolve object.';
      ERR_THREAD_GROUP_NF      = 'Thread Group "%s" not found.';
      ERR_VERSION              = 'Version mismatch, current=[%d], peer=[%d].';
      ERR_READ_STDOUT_DISABLED = 'Control I/O is disabled. Can''t read process stdout.';
      ERR_MAGIC                = 'SubSeven was not authorized to run in this system. Please contact your administrator and run SubSeven Service Controller to grant Server.';
      ERR_LIBRARY              = 'Could not load "%s" library.';
      ERR_INVALID_STORED_CAST  = 'Stored data format doesn''t match expected one. It should be "%s" format instead.';
      ERR_SERIALIZED_PROPERTIES = 'Expected serialized node(s)/propertie(s) are missing.';
      ERR_SERIALIZED_CLASS      = 'Input data class type doesn''t match expected class type.';

      {
        Logger Messages
      }
      MSG_THREADENTER = 'Thread=[%d], Class=[%s] has start.';
      MSG_THREADEXIT  = 'Thread=[%d], Class=[%s] has exit.';

      {
        Custom Windows Messages
      }
      WM_ATTACH_CLIENT  = WM_USER + 2801;
      WM_DETTACH_CLIENT = WM_ATTACH_CLIENT + 1;

implementation

end.
